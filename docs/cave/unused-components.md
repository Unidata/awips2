---
layout: default
type: guide
title: Unused Operational AWIPS Components
---

An overview of components that are used operationally by the NWS but are made inactive in the Unidata release.  Some components are impractical for non-operational use, and some are unavailable for distribution outside of the NWS.


# Data Delivery

The "Data Delivery" option opens the Data Delivery application. Data Delivery is a permission-based application, meaning that the System Manager or User Administrator controls the user's access to the Data Delivery functionalities. If granted permission to access this application, Data Delivery allows a user to subscribe to a data source or create an ad hoc request and have the data delivered in near real time. Whether delivered by subscription or in response to an ad hoc request, the data can be tailored to a user's specific temporal, geographic, and parameter needs. For a detailed description of the Data Delivery application, refer to Section 16.1.

# Collaboration

The "Collaboration" option offers two main functions: chatting and sharing displays. Chat allows users to send and receive instant messages or chat with fellow forecasters and offices in a chat room. Sharing displays adds to the chat room capabilities and allows the room's creator to show a CAVE map display to other participants in the room. For a detailed description of "Collaboration" and information on how to create a chat session and share displays, refer to Section 16.2.

# Archive Case Creation

The "Archive Case Creation" option is a component of the AWIPS-2 Archiver application. The archiver application is a permission-based functionality. It allows a user to extract stored weather event data and copy it into a user-defined directory to be archived (e.g., burned to a DVD). The archived data can later be played back for simulation of weather events using the WES-2 Bridge. For a detailed description of the AWIPS-2 Archiver application and the "Archive Case Creation" component, refer to Section 16.3. Archive Retention: The "Archive Retention" option is a component of the AWIPS-2 Archiver application. The archiver retention functionality and its purge component, which runs on EDEX, are permission-based functionalities. Access to the "Archive Retention" option is limited to User Administrators and users identified as a database/purge focal point. More information on these AWIPS-2 Archiver application functionalities are provided in the System Manager's Manual.

# AWIPS User Administration

Some of the functionalities of certain CAVE applications (currently, Data Delivery and Localization) are reserved for designated users. User Administrators choose the "AWIPS User Administration" option to access the screens they use to set permissions and roles for the reserved functions. Access to the "AWIPS User Administration" option is limited to User Administrators. Other users who select this option will be denied access and receive the Alert Message shown in Exhibit 2.2.6.1-5. More information on AWIPS User Administration is provided in the System Manager's Manual.


# LDAD (Local Data Acquisition and Dissemination)

The LDAD system provides the means to acquire local data sets, perform quality control on the incoming data, and disseminate weather data to the external user community. It contains a number of components that reside on the internal AWIPS network and on the external LDAD component (on the LDAD server cluster). The internal and external components at WFOs are separated by a security firewall.

The basic LDAD concept simplifies this process for both the data providers and for the support team. LDAD uses a simple data format, ASCII Comma Separated Values Text (CSVText), which separates each data field by a comma. A set of metadata files, created and maintained by the data provider or in conjunction with site personnel, will be used by the acquisition decoder. This facilitates data processing in hydrometeorological units instead of sensor units and removes the need for conversion routines. The simplicity of the CSVText format increases the likelihood that the data provider will use this standardized format.

All LDAD acquisition data are categorized and stored into the following four classes:
* Mesonet for surface weather observations
* Hydro for rain and stream observations
* Manual for manual observations such as cooperative observers
* Upper air for multilevel observations such as profilers. 

The LDAD functionality supports the acquisition of the Integrated Flood Observing and Warning System (IFLOWS); ALERT; Mesonet; Profiler; RRS/Upper Air; Gauges (LARC, Handar, Campbell, Sutron); COOP (Co-operative Observations); and other data transported via LDM, Rsync, or other TCP/IP Protocols. The Data Acquisition function is achieved when data is transmitted to the internal (trusted) AWIPS servers.  The data is transmitted to and from the LDAD Cluster via TCP/IP protocols or RS-232 communications. The Data Dissemination function is achieved when data is transmitted to the LDAD Cluster from the internal AWIPS system and is then distributed to External Users. The data can be acquired, stored, and displayed once fully configured. 

The LDAD System consists of two LDAD servers (LS2/3), a LAN switch (SMC 8024), a Terminal Server (Cyclades ACS32), Modems (MultiTech MT5600BR), and a LAN DMZ (HP ProCurve 2824). The DMZ consists of two SSG 320M Firewalls, a Netgear 16 switch, and two Netgear 5 port hubs. The LDAD baseline processes run on the LDAD Cluster (DMZ) and the AWIPS PX Cluster (Internal). Other local applications may run on other internal clusters, such as DX cluster in the case of the LDAD Dissemination Server. Data is transmitted through the DMZ either to the Trusted (internal) AWIPS system or to the Untrusted (External) Users/Systems. 


