---
layout: default
type: guide
shortname: Docs
title: Linux Tools
subtitle: EDEX Admin
---



## Using Standard Linux Tools 

Several standard Linux tools can be used to monitor the EDEX  processes, and for the purposes of this document and the Unidata AWIPS Training Workshop, it is assumed that all are available and that the user has some knowledge of how they are used.  Regardless, this document includes the full command syntax that can be copy and pasted from the document to the terminal.



* ps - Display information about specific processes 

  ps aux | grep edex


* cat - Used to display a text file in a terminal 

  cat /awips2/ldm/etc/pqact.conf

* tail - Used to provide a dynamic picture of process logs 

  tail -f /awips2/ldm/logs/ldmd.conf
  
* grep - Used to filter content of process logs; used to filter output of other tools 

  grep edexBridge /awips2/ldm/etc/ldmd.conf

* top - Provides a dynamic view of the memory and cpu usage of the EDEX processes 

* psql - A terminal-based front-end to PostgreSQL.  We will be executing SQL queries.  You do not need to have previous experience with SQL to follow this guide, but navigating AWIPS metadata is made much easier with some experience.


 [awips@edex ~]$ psql metadata
  psql (9.2.4)
  Type "help" for help.
  
  metadata=# help
  You are using psql, the command-line interface to PostgreSQL.
  Type:  \copyright for distribution terms
         \h for help with SQL commands
         \? for help with psql commands
         \g or terminate with semicolon to execute query
         \q to quit
  metadata=# \dt sat*
                       List of relations
   Schema |               Name                | Type  | Owner 
  --------+-----------------------------------+-------+-------
   awips  | satellite                         | table | awips
   awips  | satellite_creating_entities       | table | awips
   awips  | satellite_geostationary_positions | table | awips
   awips  | satellite_physical_elements       | table | awips
   awips  | satellite_sector_ids              | table | awips
   awips  | satellite_sources                 | table | awips
   awips  | satellite_spatial                 | table | awips
   awips  | satellite_units                   | table | awips
  (8 rows)
  
  metadata=# \q



