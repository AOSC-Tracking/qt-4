/****************************************************************************
**
** Copyright (C) 2011 Nokia Corporation and/or its subsidiary(-ies).
** All rights reserved.
** Contact: Nokia Corporation (qt-info@nokia.com)
**
** This file is part of the QtGui module of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser General Public
** License version 2.1 as published by the Free Software Foundation and
** appearing in the file LICENSE.LGPL included in the packaging of this
** file. Please review the following information to ensure the GNU Lesser
** General Public License version 2.1 requirements will be met:
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Nokia gives you certain additional
** rights. These rights are described in the Nokia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU General
** Public License version 3.0 as published by the Free Software Foundation
** and appearing in the file LICENSE.GPL included in the packaging of this
** file. Please review the following information to ensure the GNU General
** Public License version 3.0 requirements will be met:
** http://www.gnu.org/copyleft/gpl.html.
**
** Other Usage
** Alternatively, this file may be used in accordance with the terms and
** conditions contained in a signed written agreement between you and Nokia.
**
**
**
**
**
** $QT_END_LICENSE$
**
****************************************************************************/
#include "qxembedsystemtrayicon_x11_p.h"

#ifndef QT_NO_SYSTEMTRAYICON

#include "private/qt_x11_p.h"
#include "qapplication.h"
#include "qevent.h"
#include "qlist.h"
#include "qmenu.h"
#include "qpainter.h"
#include "qpaintengine.h"
#include "qsystemtrayicon_p.h"
#include "qx11info_x11.h"

QT_BEGIN_INCLUDE_NAMESPACE
#include <QtCore/qcoreapplication.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
QT_END_INCLUDE_NAMESPACE

QT_BEGIN_NAMESPACE

class QSystemTrayIconWidget : public QWidget
{
public:
    QSystemTrayIconWidget(QSystemTrayIcon *q, QXEmbedSystemTrayIconSys *s);
    ~QSystemTrayIconWidget();

    static Window locateSystemTray();

protected:
    void paintEvent(QPaintEvent *pe);
    void resizeEvent(QResizeEvent *re);
    bool x11Event(XEvent *event);
    void mousePressEvent(QMouseEvent *event);
    void mouseDoubleClickEvent(QMouseEvent *event);
#ifndef QT_NO_WHEELEVENT
    void wheelEvent(QWheelEvent *event);
#endif
    bool event(QEvent *e);

private:
    enum {
        SYSTEM_TRAY_REQUEST_DOCK = 0,
        SYSTEM_TRAY_BEGIN_MESSAGE = 1,
        SYSTEM_TRAY_CANCEL_MESSAGE =2
    };

    void addToTray();
    static XVisualInfo* getSysTrayVisualInfo();

    static Window sysTrayWindow;
    static QList<QSystemTrayIconWidget *> trayIcons;
    static QCoreApplication::EventFilter oldEventFilter;
    static bool sysTrayTracker(void *message, long *result);
    static Atom sysTraySelection;
    static XVisualInfo sysTrayVisual;

    QSystemTrayIcon *q;
    QXEmbedSystemTrayIconSys *sys;
    Colormap colormap;
};

Window QSystemTrayIconWidget::sysTrayWindow = XNone;
QList<QSystemTrayIconWidget *> QSystemTrayIconWidget::trayIcons;
QCoreApplication::EventFilter QSystemTrayIconWidget::oldEventFilter = 0;
Atom QSystemTrayIconWidget::sysTraySelection = XNone;
XVisualInfo QSystemTrayIconWidget::sysTrayVisual = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

QSystemTrayIconWidget::QSystemTrayIconWidget(QSystemTrayIcon* q, QXEmbedSystemTrayIconSys* sys)
: QWidget(0, Qt::FramelessWindowHint | Qt::X11BypassWindowManagerHint)
, q(q)
, sys(sys)
, colormap(0)
{
    setAttribute(Qt::WA_AlwaysShowToolTips);
    setAttribute(Qt::WA_QuitOnClose, false);
    setAttribute(Qt::WA_NoSystemBackground, true);
    setAttribute(Qt::WA_PaintOnScreen);
    setMouseTracking(true);
#ifndef QT_NO_TOOLTIP
    setToolTip(q->toolTip());
#endif

    static bool eventFilterAdded = false;
    Display *display = QX11Info::display();
    if (!eventFilterAdded) {
        oldEventFilter = qApp->setEventFilter(sysTrayTracker);
        eventFilterAdded = true;
        Window root = QX11Info::appRootWindow();
        XWindowAttributes attr;
        XGetWindowAttributes(display, root, &attr);
        if ((attr.your_event_mask & StructureNotifyMask) != StructureNotifyMask) {
            (void) QApplication::desktop(); // lame trick to ensure our event mask is not overridden
            XSelectInput(display, root, attr.your_event_mask | StructureNotifyMask); // for MANAGER selection
        }
    }
    if (trayIcons.isEmpty()) {
        sysTrayWindow = locateSystemTray();
        if (sysTrayWindow != XNone)
            XSelectInput(display, sysTrayWindow, StructureNotifyMask); // track tray events
    }
    trayIcons.append(this);
    if (sysTrayWindow != XNone)
        addToTray();
}

QSystemTrayIconWidget::~QSystemTrayIconWidget()
{
    trayIcons.removeAt(trayIcons.indexOf(this));
    Display *display = QX11Info::display();
    if (trayIcons.isEmpty()) {
        if (sysTrayWindow == XNone)
            return;
        if (display)
            XSelectInput(display, sysTrayWindow, 0); // stop tracking the tray
        sysTrayWindow = XNone;
    }
    if (colormap)
        XFreeColormap(display, colormap);
}

void QSystemTrayIconWidget::resizeEvent(QResizeEvent *re)
{
    QWidget::resizeEvent(re);
    update();
}

void QSystemTrayIconWidget::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    if (!getSysTrayVisualInfo()) {
        const QRegion oldSystemClip = p.paintEngine()->systemClip();
        const QRect clearedRect = oldSystemClip.boundingRect();
        XClearArea(QX11Info::display(), winId(), clearedRect.x(), clearedRect.y(),
                   clearedRect.width(), clearedRect.height(), False);
        QPaintEngine *pe = p.paintEngine();
        pe->setSystemClip(clearedRect);
        q->icon().paint(&p, rect());
        pe->setSystemClip(oldSystemClip);
    } else {
        p.setCompositionMode(QPainter::CompositionMode_Source);
        p.fillRect(rect(), Qt::transparent);
        p.setCompositionMode(QPainter::CompositionMode_SourceOver);
        q->icon().paint(&p, rect());
    }
}

void QSystemTrayIconWidget::mousePressEvent(QMouseEvent *ev)
{
    QPoint globalPos = ev->globalPos();
    if (ev->button() == Qt::RightButton && q->contextMenu())
        q->contextMenu()->popup(globalPos);

    if (QBalloonTip::isBalloonVisible()) {
        QMetaObject::invokeMethod(q, "messageClicked");
        QBalloonTip::hideBalloon();
    }

    if (ev->button() == Qt::LeftButton)
        qtsystray_sendActivated(q, QSystemTrayIcon::Trigger);
    else if (ev->button() == Qt::RightButton)
        qtsystray_sendActivated(q, QSystemTrayIcon::Context);
    else if (ev->button() == Qt::MidButton)
        qtsystray_sendActivated(q, QSystemTrayIcon::MiddleClick);
}

void QSystemTrayIconWidget::mouseDoubleClickEvent(QMouseEvent *ev)
{
    if (ev->button() == Qt::LeftButton)
        qtsystray_sendActivated(q, QSystemTrayIcon::DoubleClick);
}

#ifndef QT_NO_WHEELEVENT
void QSystemTrayIconWidget::wheelEvent(QWheelEvent *e)
{
    sys->sendWheelEventToTrayIcon(e->delta(), e->orientation());
}
#endif

bool QSystemTrayIconWidget::event(QEvent *e)
{
    if (e->type() == QEvent::ToolTip) {
        sys->sendToolTipEventToTrayIcon();
    }
    return QWidget::event(e);
}

bool QSystemTrayIconWidget::x11Event(XEvent *event)
{
    if (event->type == ReparentNotify)
        show();
    return QWidget::x11Event(event);
}

// Locate the system tray
Window QSystemTrayIconWidget::locateSystemTray()
{
    Display *display = QX11Info::display();
    if (sysTraySelection == XNone) {
        int screen = QX11Info::appScreen();
        QString net_sys_tray = QString::fromLatin1("_NET_SYSTEM_TRAY_S%1").arg(screen);
        sysTraySelection = XInternAtom(display, net_sys_tray.toLatin1(), False);
    }

    return XGetSelectionOwner(QX11Info::display(), sysTraySelection);
}

XVisualInfo* QSystemTrayIconWidget::getSysTrayVisualInfo()
{
    Display *display = QX11Info::display();

    if (!sysTrayVisual.visual) {
        Window win = locateSystemTray();
        if (win != XNone) {
            Atom actual_type;
            int actual_format;
            ulong nitems, bytes_remaining;
            uchar *data = 0;
            int result = XGetWindowProperty(display, win, ATOM(_NET_SYSTEM_TRAY_VISUAL), 0, 1,
                                            False, XA_VISUALID, &actual_type,
                                            &actual_format, &nitems, &bytes_remaining, &data);
            VisualID vid = 0;
            if (result == Success && data && actual_type == XA_VISUALID && actual_format == 32 &&
                nitems == 1 && bytes_remaining == 0)
                vid = *(VisualID*)data;
            if (data)
                XFree(data);
            if (vid == 0)
                return 0;

            uint mask = VisualIDMask;
            XVisualInfo *vi, rvi;
            int count;
            rvi.visualid = vid;
            vi = XGetVisualInfo(display, mask, &rvi, &count);
            if (vi) {
                sysTrayVisual = vi[0];
                XFree((char*)vi);
            }
            if (sysTrayVisual.depth != 32)
                memset(&sysTrayVisual, 0, sizeof(sysTrayVisual));
        }
    }

    return sysTrayVisual.visual ? &sysTrayVisual : 0;
}

bool QSystemTrayIconWidget::sysTrayTracker(void *message, long *result)
{
    bool retval = false;
    if (QSystemTrayIconWidget::oldEventFilter)
        retval = QSystemTrayIconWidget::oldEventFilter(message, result);

    if (trayIcons.isEmpty())
        return retval;

    Display *display = QX11Info::display();
    XEvent *ev = (XEvent *)message;
    if  (ev->type == DestroyNotify && ev->xany.window == sysTrayWindow) {
        sysTrayWindow = locateSystemTray();
        memset(&sysTrayVisual, 0, sizeof(sysTrayVisual));
        for (int i = 0; i < trayIcons.count(); i++) {
            if (sysTrayWindow == XNone) {
                QBalloonTip::hideBalloon();
                trayIcons[i]->hide(); // still no luck
                trayIcons[i]->destroy();
                trayIcons[i]->create();
            } else
                trayIcons[i]->addToTray(); // add it to the new tray
        }
        retval = true;
    } else if (ev->type == ClientMessage && sysTrayWindow == XNone) {
        static Atom manager_atom = XInternAtom(display, "MANAGER", False);
        XClientMessageEvent *cm = (XClientMessageEvent *)message;
        if ((cm->message_type == manager_atom) && ((Atom)cm->data.l[1] == sysTraySelection)) {
            sysTrayWindow = cm->data.l[2];
            memset(&sysTrayVisual, 0, sizeof(sysTrayVisual));
            XSelectInput(display, sysTrayWindow, StructureNotifyMask);
            for (int i = 0; i < trayIcons.count(); i++) {
                trayIcons[i]->addToTray();
            }
            retval = true;
        }
    } else if (ev->type == PropertyNotify && ev->xproperty.atom == ATOM(_NET_SYSTEM_TRAY_VISUAL) &&
               ev->xproperty.window == sysTrayWindow) {
        memset(&sysTrayVisual, 0, sizeof(sysTrayVisual));
        for (int i = 0; i < trayIcons.count(); i++) {
            trayIcons[i]->addToTray();
        }
    }

    return retval;
}

void QSystemTrayIconWidget::addToTray()
{
    Q_ASSERT(sysTrayWindow != XNone);
    Display *display = QX11Info::display();

    XVisualInfo *vi = getSysTrayVisualInfo();
    if (vi && vi->visual) {
        Window root = RootWindow(display, vi->screen);
        Window p = root;
        if (QWidget *pw = parentWidget())
            p = pw->effectiveWinId();
        colormap = XCreateColormap(display, root, vi->visual, AllocNone);
        XSetWindowAttributes wsa;
        wsa.background_pixmap = 0;
        wsa.colormap = colormap;
        wsa.background_pixel = 0;
        wsa.border_pixel = 0;
        Window wid = XCreateWindow(display, p, -1, -1, 1, 1,
                                   0, vi->depth, InputOutput, vi->visual,
                                   CWBackPixmap|CWBackPixel|CWBorderPixel|CWColormap, &wsa);
        create(wid);
    } else {
        XSetWindowBackgroundPixmap(display, winId(), ParentRelative);
    }

    // GNOME, NET WM Specification
    static Atom netwm_tray_atom = XInternAtom(display, "_NET_SYSTEM_TRAY_OPCODE", False);
    long l[5] = { CurrentTime, SYSTEM_TRAY_REQUEST_DOCK, static_cast<long>(winId()), 0, 0 };
    XEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.xclient.type = ClientMessage;
    ev.xclient.window = sysTrayWindow;
    ev.xclient.message_type = netwm_tray_atom;
    ev.xclient.format = 32;
    memcpy((char *)&ev.xclient.data, (const char *) l, sizeof(l));
    XSendEvent(display, sysTrayWindow, False, 0, &ev);
    setMinimumSize(22, 22); // required at least on GNOME
}

////////////////////////////////////////////////////////////////////////////
QXEmbedSystemTrayIconSys::QXEmbedSystemTrayIconSys(QSystemTrayIcon *q)
: QAbstractSystemTrayIconSys(q)
, widget(0)
{
}

QXEmbedSystemTrayIconSys::~QXEmbedSystemTrayIconSys()
{
    delete widget;
}

QRect QXEmbedSystemTrayIconSys::geometry() const
{
    if (!widget)
        return QRect();
    return QRect(widget->mapToGlobal(QPoint(0, 0)), widget->size());
}

void QXEmbedSystemTrayIconSys::updateIcon()
{
    if (!widget)
        return;
    widget->update();
}

void QXEmbedSystemTrayIconSys::updateToolTip()
{
    if (!widget)
        return;
    widget->setToolTip(trayIcon->toolTip());
}

void QXEmbedSystemTrayIconSys::showMessage(const QString &message, const QString &title,
                                   QSystemTrayIcon::MessageIcon icon, int msecs)
{
    if (!widget)
        return;
    QPoint point = geometry().center();
    QBalloonTip::showBalloon(icon, message, title, trayIcon, point, msecs);
}

void QXEmbedSystemTrayIconSys::updateVisibility()
{
    bool visible = trayIcon->isVisible();
    if (visible && !widget)
        widget = new QSystemTrayIconWidget(trayIcon, this);
    else if (!visible && widget) {
        delete widget;
        widget = 0;
    }
}

void QXEmbedSystemTrayIconSys::sendToolTipEventToTrayIcon()
{
#ifndef QT_NO_TOOLTIP
    // Pass the event through QSystemTrayIcon so that it gets a chance to
    // update the tooltip, then asks widget to show the tooltip
    Q_ASSERT(widget);
    QPoint globalPos = QCursor::pos();
    QPoint pos = widget->mapFromGlobal(globalPos);
    QHelpEvent event(QEvent::ToolTip, pos, globalPos);
    QApplication::sendEvent(trayIcon, &event);
#endif
}

void QXEmbedSystemTrayIconSys::sendWheelEventToTrayIcon(int delta, Qt::Orientation orientation)
{
#ifndef QT_NO_WHEELEVENT
    Q_ASSERT(widget);
    QPoint globalPos = QCursor::pos();
    QPoint pos = widget->mapFromGlobal(globalPos);
    QWheelEvent event(pos, globalPos, delta, Qt::NoButton, Qt::NoModifier, orientation);
    QApplication::sendEvent(trayIcon, &event);
#endif
}

void QXEmbedSystemTrayIconSys::updateMenu()
{
}

/////////////////////////////////////////////////////////////
QAbstractSystemTrayIconSys * QXEmbedSystemTrayIconSysFactory::create(QSystemTrayIcon *icon)
{
    return new QXEmbedSystemTrayIconSys(icon);
}

bool QXEmbedSystemTrayIconSysFactory::isAvailable() const
{
    return QSystemTrayIconWidget::locateSystemTray() != XNone;
}

QT_END_NAMESPACE
#endif //QT_NO_SYSTEMTRAYICON
