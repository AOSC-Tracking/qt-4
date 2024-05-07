/****************************************************************************
**
** Copyright (C) 2015 The Qt Company Ltd.
** Contact: http://www.qt.io/licensing/
**
** This file is part of the QtCore module of the Qt Toolkit.
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

#include "qcore_unix_p.h"
#include "qelapsedtimer.h"

#ifdef Q_OS_NACL
#elif !defined (Q_OS_VXWORKS)
# if !defined(Q_OS_HPUX) || defined(__ia64)
#  include <sys/select.h>
# endif
#  include <sys/time.h>
#else
#  include <selectLib.h>
#endif

#include <stdlib.h>

#ifdef Q_OS_MAC
#include <mach/mach_time.h>
#endif

QT_BEGIN_NAMESPACE

static inline bool time_update(struct timeval *tv, const struct timeval &start,
                               const struct timeval &timeout)
{
    // clock source is (hopefully) monotonic, so we can recalculate how much timeout is left;
    // if it isn't monotonic, we'll simply hope that it hasn't jumped, because we have no alternative
    struct timeval now = qt_gettime();
    *tv = timeout + start - now;
    return tv->tv_sec >= 0;
}

int qt_safe_select(int nfds, fd_set *fdread, fd_set *fdwrite, fd_set *fdexcept,
                   const struct timeval *orig_timeout)
{
    if (!orig_timeout) {
        // no timeout -> block forever
        int ret;
        EINTR_LOOP(ret, select(nfds, fdread, fdwrite, fdexcept, 0));
        return ret;
    }

    timeval start = qt_gettime();
    timeval timeout = *orig_timeout;

    // loop and recalculate the timeout as needed
    int ret;
    forever {
        ret = ::select(nfds, fdread, fdwrite, fdexcept, &timeout);
        if (ret != -1 || errno != EINTR)
            return ret;

        // recalculate the timeout
        if (!time_update(&timeout, start, *orig_timeout)) {
            // timeout during update
            // or clock reset, fake timeout error
            return 0;
        }
    }
}

#ifndef Q_OS_VXWORKS

int qt_safe_poll(struct pollfd *fds, int nfds, int timeout_ms, bool retry_eintr)
{
    if (nfds == 0)
	return 0;
    if (nfds < 0) {
	errno = EINVAL;
	return -1;
    }

    // Retry on ret == 0 if the deadline has not yet passed because
    // Linux can return early from the syscall, without setting EINTR.
    if (timeout_ms < 0) {
	forever {
	    int ret = ::poll(fds, nfds, -1);
	    if (ret > 0)
		return ret;
	    if (retry_eintr) {
		if (ret == 0 || ret == -1 && errno == EINTR) {
		    continue;
		} else {
		    return -1;
		}
	    }
	    if (ret == 0) {
		errno = EINTR;
		return -1;
	    }
	    return ret;
	}
    }

    timeval previous = qt_gettime();
    timeval deadline = previous;
    deadline.tv_sec += timeout_ms / 1000;
    deadline.tv_usec += (timeout_ms % 1000) * 1000;
    if (deadline.tv_usec >= 1000000) {
	++deadline.tv_sec;
	deadline.tv_usec -= 1000000;
    }
    int remaining = timeout_ms;

    forever {
	int ret = ::poll(fds, nfds, remaining);
	if (ret > 0)
	    return ret;
	timeval now = qt_gettime();
	if ((now.tv_sec > deadline.tv_sec // past deadline
	     || (now.tv_sec == deadline.tv_sec
		 && now.tv_usec >= deadline.tv_usec))
	    || (now.tv_sec < previous.tv_sec // time warp
		|| (now.tv_sec == previous.tv_sec
		    && now.tv_usec < previous.tv_usec))
	    || (ret < 0 && (errno != EINTR || !retry_eintr))) // other error
	    return ret;
	if (ret == 0 && !retry_eintr) {
	    errno = EINTR;
	    return -1;
	}
        remaining = (deadline.tv_sec - now.tv_sec) * 1000
		     + (deadline.tv_usec - now.tv_usec) / 1000;
	previous = now;
    }
}

#else

// Poll emulation for VxWorks.

static int mark_bad_descriptors(pollfd *fds, int nfds)
{
    fd_set r;
    FD_ZERO(&r);
    struct timeval tv;
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    int ret = 0;

    // Check each descriptor invidually for badness.
    for (int i = 0; i < nfds; ++i) {
        pollfd &fd(fds[i]);
        if (fd.fd >= 0) {
            FD_SET(fd.fd, &r);
            int ret = qt_safe_select(fd.fd + 1, &r, NULL, NULL, &tv);
            FD_CLR(fd.fd, &r);
            if (ret < 0 && errno == EBADF) {
                fd.revents = POLLNVAL;
                ++ret;
            }
        }
    }
    Q_ASSERT(ret > 0);
    return ret;
}

int qt_safe_poll(pollfd *fds, int nfds, int timeout, bool retry_eintr)
{
    fd_set r, w;
    FD_ZERO(&r);
    FD_ZERO(&w);
    int maxfd = -1;

    // Extract the watched descriptors.
    for (int i = 0; i < nfds; ++i) {
        pollfd &fd(fds[i]);
        if (fd.fd >= 0 && fd.fd < FD_SETSIZE) {
            if (fd.events & POLLIN) {
                FD_SET(fd.fd, &r);
                if (fd.fd > maxfd)
                    maxfd = fd.fd;
            }
            if (fd.events & POLLOUT) {
                FD_SET(fd.fd, &w);
                if (fd.fd > maxfd)
                    maxfd = fd.fd;
            }
        }
    }

    // If timeout is negative, wait indefinitely for activity.
    timeval tv;
    timeval *ptv;
    if (timeout >= 0) {
        tv.tv_sec = timeout / 1000;
        tv.tv_usec = (timeout % 1000) * 1000;
        ptv = &tv;
    } else
        ptv = NULL;

    int ret;
    if (retry_eintr)
        ret = qt_safe_select(maxfd + 1, &r, &w, NULL, ptv);
    else
        ret = ::select(maxfd + 1, &r, &w, NULL, ptv);
    if (ret < 0 && errno == EBADF) {
        return mark_bad_descriptors(fds, nfds);
    }
    if (ret <= 0)
        return ret;

    // Set the revents flags.
    ret = 0;
    for (int i = 0; i < nfds; ++i) {
        pollfd &fd(fds[i]);
        fd.revents = 0;
        if (fd.fd >= 0 && fd.fd < FD_SETSIZE) {
            if ((fd.events & POLLIN) && FD_ISSET(fd.fd, &r))
                fd.revents |= POLLIN;
            if ((fd.events & POLLOUT) && FD_ISSET(fd.fd, &w))
                fd.revents |= POLLOUT;
            if (fd.revents)
                ++ret;
        }
    }
    Q_ASSERT(ret > 0);
    return ret;
}

#endif

QT_END_NAMESPACE
