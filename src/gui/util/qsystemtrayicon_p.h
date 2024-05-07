/****************************************************************************
**
** Copyright (C) 2015 The Qt Company Ltd.
** Contact: http://www.qt.io/licensing/
**
** This file is part of the QtGui module of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see http://www.qt.io/terms-conditions. For further
** information use the contact form at http://www.qt.io/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file. Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** As a special exception, The Qt Company gives you certain additional
** rights. These rights are described in The Qt Company LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/copyleft/gpl.html.
**
** $QT_END_LICENSE$
**
****************************************************************************/

#ifndef QSYSTEMTRAYICON_P_H
#define QSYSTEMTRAYICON_P_H

//
//  W A R N I N G
//  -------------
//
// This file is not part of the Qt API.  It exists for the convenience
// of a number of Qt sources files.  This header file may change from
// version to version without notice, or even be removed.
//
// We mean it.
//

#include "qsystemtrayicon.h"
#include "private/qobject_p.h"

#ifndef QT_NO_SYSTEMTRAYICON

#include "QtGui/qmenu.h"
#include "QtGui/qpixmap.h"
#include "QtCore/qstring.h"
#include "QtCore/qpointer.h"
#if defined(Q_WS_X11)
#include "QtCore/qset.h"
#endif

QT_BEGIN_NAMESPACE

#if defined(Q_WS_X11)
class QAbstractSystemTrayIconSys;
#else
class QSystemTrayIconSys;
#endif
class QToolButton;
class QLabel;

class QSystemTrayIconPrivate : public QObjectPrivate
{
    Q_DECLARE_PUBLIC(QSystemTrayIcon)

public:
    QSystemTrayIconPrivate() : sys(0), visible(false) { }
    #if defined(Q_WS_X11)
    ~QSystemTrayIconPrivate();
    #endif

    void install_sys();
    void remove_sys();
    void updateIcon_sys();
    void updateToolTip_sys();
    void updateMenu_sys();
    QRect geometry_sys() const;
    void showMessage_sys(const QString &msg, const QString &title, QSystemTrayIcon::MessageIcon icon, int secs);

    static bool isSystemTrayAvailable_sys();
    static bool supportsMessages_sys();

    QPointer<QMenu> menu;
    QIcon icon;
    QString toolTip;
    #if defined(Q_WS_X11)
    QAbstractSystemTrayIconSys *sys;
    #else
    QSystemTrayIconSys *sys;
    #endif
    bool visible;
};

class QBalloonTip : public QWidget
{
    Q_OBJECT
public:
    static void showBalloon(QSystemTrayIcon::MessageIcon icon, const QString& title,
                            const QString& msg, QSystemTrayIcon *trayIcon,
                            const QPoint& pos, int timeout, bool showArrow = true);
    static void hideBalloon();
    static bool isBalloonVisible();

private:
    QBalloonTip(QSystemTrayIcon::MessageIcon icon, const QString& title,
                const QString& msg, QSystemTrayIcon *trayIcon);
    ~QBalloonTip();
    void balloon(const QPoint&, int, bool);

protected:
    void paintEvent(QPaintEvent *);
    void resizeEvent(QResizeEvent *);
    void mousePressEvent(QMouseEvent *e);
    void timerEvent(QTimerEvent *e);

private:
    QSystemTrayIcon *trayIcon;
    QPixmap pixmap;
    int timerId;
};

#if defined(Q_WS_X11)
class QSystemTrayIconSysFactoryInterface;

/**
 * This class acts as a composite QSystemTrayIconSysFactory: It can create
 * instances of QAbstractSystemTrayIconSys* using either a plugin or the
 * builtin factory and will cause QSystemTrayIconPrivate to recreate their
 * 'sys' instances if the plugin availability changes.
 */
class QSystemTrayIconSysFactory : public QObject
{
    Q_OBJECT
public:
    QSystemTrayIconSysFactory();
    void registerSystemTrayIconPrivate(QSystemTrayIconPrivate *iconPrivate);
    void unregisterSystemTrayIconPrivate(QSystemTrayIconPrivate *iconPrivate);

    QAbstractSystemTrayIconSys *create(QSystemTrayIcon *) const;

    bool isAvailable() const;

private Q_SLOTS:
    void refreshTrayIconPrivates();

private:
    QSystemTrayIconSysFactoryInterface *factory() const;
    void loadPluginFactory();

    QSystemTrayIconSysFactoryInterface *pluginFactory;
    QSet<QSystemTrayIconPrivate *> trayIconPrivates;
};
#endif

QT_END_NAMESPACE

#endif // QT_NO_SYSTEMTRAYICON

#endif // QSYSTEMTRAYICON_P_H

