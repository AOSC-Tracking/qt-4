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

#ifndef QXEMBEDSYSTEMTRAYICON_X11_P_H
#define QXEMBEDSYSTEMTRAYICON_X11_P_H

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

#ifndef QT_NO_SYSTEMTRAYICON

#include "qabstractsystemtrayiconsys_p.h"

QT_BEGIN_NAMESPACE

class QSystemTrayIconWidget;

class QXEmbedSystemTrayIconSys : public QAbstractSystemTrayIconSys
{
public:
    QXEmbedSystemTrayIconSys(QSystemTrayIcon *);
    ~QXEmbedSystemTrayIconSys();

    QRect geometry() const;

    void updateVisibility();

    void updateIcon();

    void updateToolTip();

    void updateMenu();

    void showMessage(const QString &message, const QString &title,
                     QSystemTrayIcon::MessageIcon icon, int msecs);

private:
    friend class QSystemTrayIconWidget;
    QSystemTrayIconWidget *widget;

    void sendToolTipEventToTrayIcon();

    void sendWheelEventToTrayIcon(int delta, Qt::Orientation orientation);
};

struct QXEmbedSystemTrayIconSysFactory : public QSystemTrayIconSysFactoryInterface
{
    QAbstractSystemTrayIconSys * create(QSystemTrayIcon *trayIcon);
    bool isAvailable() const;
};


QT_END_NAMESPACE

#endif // QT_NO_SYSTEMTRAYICON

#endif // QXEMBEDSYSTEMTRAYICON_X11_P_H

