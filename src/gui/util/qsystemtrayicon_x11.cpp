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
#ifndef QT_NO_SYSTEMTRAYICON

#include <private/qfactoryloader_p.h>

#include "qsystemtrayicon_p.h"
#include "qabstractsystemtrayiconsys_p.h"
#include "qcoreapplication.h"
#include "qxembedsystemtrayicon_x11_p.h"

QT_BEGIN_NAMESPACE

Q_GLOBAL_STATIC(QSystemTrayIconSysFactory, qt_guiSystemTrayIconSysFactory)

QSystemTrayIconSysFactory::QSystemTrayIconSysFactory()
: pluginFactory(0)
{
}

void QSystemTrayIconSysFactory::loadPluginFactory()
{
    if (pluginFactory) {
        return;
    }
#ifndef QT_NO_LIBRARY
    QFactoryLoader loader(QSystemTrayIconSysFactoryInterface_iid, QLatin1String("/systemtrayicon"));
    pluginFactory = qobject_cast<QSystemTrayIconSysFactoryInterface *>(loader.instance(QLatin1String("default")));
    if (pluginFactory) {
        // Set parent to ensure factory destructor is called when application
        // is closed
        pluginFactory->setParent(QCoreApplication::instance());
        connect(pluginFactory, SIGNAL(availableChanged(bool)), SLOT(refreshTrayIconPrivates()));
    }
#endif // QT_NO_LIBRARY
}

QSystemTrayIconSysFactoryInterface *QSystemTrayIconSysFactory::factory() const
{
    if (!pluginFactory) {
        const_cast<QSystemTrayIconSysFactory*>(this)->loadPluginFactory();
    }
    if (pluginFactory && pluginFactory->isAvailable()) {
        return pluginFactory;
    }
    static QXEmbedSystemTrayIconSysFactory def;
    return def.isAvailable() ? &def : 0;
}

void QSystemTrayIconSysFactory::refreshTrayIconPrivates()
{
    Q_FOREACH(QSystemTrayIconPrivate *trayIconPrivate, trayIconPrivates) {
        if (trayIconPrivate->sys) {
            delete trayIconPrivate->sys;
            trayIconPrivate->sys = 0;
        }
        // When visible is true, sys is usually not 0 but it can be 0 if the
        // call to install_sys() failed.
        if (trayIconPrivate->visible) {
            trayIconPrivate->install_sys();
        }
    }
}

void QSystemTrayIconSysFactory::registerSystemTrayIconPrivate(QSystemTrayIconPrivate* trayIconPrivate)
{
    trayIconPrivates.insert(trayIconPrivate);
}

void QSystemTrayIconSysFactory::unregisterSystemTrayIconPrivate(QSystemTrayIconPrivate* trayIconPrivate)
{
    trayIconPrivates.remove(trayIconPrivate);
}

QAbstractSystemTrayIconSys *QSystemTrayIconSysFactory::create(QSystemTrayIcon *trayIcon) const
{
    QSystemTrayIconSysFactoryInterface *f = factory();
    if (!f) {
        qWarning("No systemtrayicon available");
        return 0;
    }
    return f->create(trayIcon);
}

bool QSystemTrayIconSysFactory::isAvailable() const
{
    return factory();
}

////////////////////////////////////////////////
QSystemTrayIconPrivate::~QSystemTrayIconPrivate()
{
    qt_guiSystemTrayIconSysFactory()->unregisterSystemTrayIconPrivate(this);
    delete sys;
}

void QSystemTrayIconPrivate::install_sys()
{
    Q_Q(QSystemTrayIcon);
    if (!sys) {
        // Register ourself even if create() fails: our "sys" will get created
        // later by refreshTrayIconPrivates() if a systemtray becomes
        // available. This situation can happen for applications which are
        // started at login time, while the desktop itself is starting up.
        qt_guiSystemTrayIconSysFactory()->registerSystemTrayIconPrivate(this);
        sys = qt_guiSystemTrayIconSysFactory()->create(q);
        if (!sys) {
            return;
        }
    }
    sys->updateVisibility();
}

QRect QSystemTrayIconPrivate::geometry_sys() const
{
    if (!sys || !visible)
        return QRect();
    return sys->geometry();
}

void QSystemTrayIconPrivate::remove_sys()
{
    if (!sys)
        return;
    QBalloonTip::hideBalloon();
    sys->updateVisibility();
}

void QSystemTrayIconPrivate::updateIcon_sys()
{
    if (!sys || !visible)
        return;
    sys->updateIcon();
}

void QSystemTrayIconPrivate::updateMenu_sys()
{
    if (!sys || !visible)
        return;
    sys->updateMenu();
}

void QSystemTrayIconPrivate::updateToolTip_sys()
{
    if (!sys || !visible)
        return;
#ifndef QT_NO_TOOLTIP
    sys->updateToolTip();
#endif
}

bool QSystemTrayIconPrivate::isSystemTrayAvailable_sys()
{
    return qt_guiSystemTrayIconSysFactory()->isAvailable();
}

bool QSystemTrayIconPrivate::supportsMessages_sys()
{
    return true;
}

void QSystemTrayIconPrivate::showMessage_sys(const QString &message, const QString &title,
                                   QSystemTrayIcon::MessageIcon icon, int msecs)
{
    if (!sys || !visible)
        return;
    sys->showMessage(message, title, icon, msecs);
}

QT_END_NAMESPACE
#endif //QT_NO_SYSTEMTRAYICON
