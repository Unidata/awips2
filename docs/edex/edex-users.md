# Monitor Users

To see a list of clients connecting to your EDEX server, use the `edex users [YYYYMMDD]` command, where `[YYYYMMDD]` is the optional date string.

    edex users
    
     -- EDEX Users 20160826 --
    user@101.253.20.225
    user@192.168.1.67
    awips@0.0.0.0
    awips@sdsmt.edu
    ...


    
## Logging Daily EDEX Users

To get a running log of who has accessed EDEX, you can create a short script.  

The example below is a script that runs once daily at 20 minutes after 00 UTC, appending each day's `edex users` list to a logfile `/home/awips/edex-users.log`:


1. 
        vi~/edexUsers.sh
        
        #!/bin/bash
        /awips2/edex/bin/edex users >> /home/awips/edex-users.log
        
2. 
    crontab -e
        
        0 20 * * * /home/awips/edexUsers.sh 1>> /dev/null 2>&1
    
    
