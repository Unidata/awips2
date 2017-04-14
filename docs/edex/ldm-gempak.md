---
layout: default
type: guide
shortname: Docs
title: LDM for AWIPS and GEMPAK
subtitle: EDEX Admin
---

# LDM for AWIPS and GEMPAK

It is possible to have two LDM installs (since AWIPS LDM installs to `/awips2/ldm` and is owned and run by user **awips:awips**.  But two LDM clients doubles your bandwidth and uncessary.  This document explains how the LDM keeps its EDEX processing separate from other processing (GEMPAK decoders, TDS, etc.).

### /awips2/ldm/etc/ldmd.conf

The defailt AWIPS LDM config file executes a single **pqact** process with the **-e** flag (for EDEX).  The **edexBridge** server name is defined (typically the local machine name):

    EXEC    "pqact -e /awips2/ldm/etc/pqact.conf"
    EXEC    "edexBridge -s $HOSTNAME"
   
A separate **EXEC** line should be added for GEMPAK decoders, *without* the **-e** flag:

    EXEC    "pqact -f IDS-DDPLUS /awips2/ldm/etc/pqact.gempak_decoders"

yum install apr-devel apr-util-devel "libdb-4.7.so()(64bit)"

