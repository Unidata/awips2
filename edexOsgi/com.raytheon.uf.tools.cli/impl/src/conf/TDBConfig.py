##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##############################################################################
# Contains configuration information specific to the Subscription Manager tool
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#                                                 Initial Creation.
#    Feb 23, 2016    4716          rferrel        Added help for AWIPS commands.
##############################################################################

##############################################################################
##
# Contains configuration information specific to the textdb tool
#
# service end-point definition dictionary:
# basic format is
#       'token':'endpoint'
endpoint = {'textdb':'/services/textdbsrv'}

# Command line flag decoder dictionary:
# Basic format is
#       'flag':('command','help/usage text'
flags = {'-r':('text_get_prod','Read data from the database'),
         'read':('text_get_prod','Read data from the database'),
         '-rd':('text_get_info','Display latest product with product count and product lengths'),
         '-rk':('text_getAwips_prod', 'Read data from the database using AWIPS command'),
         '-rkd':('text_getAwips_info', 'Display using AWIPS command with product count and product lengths'),
         
         '-rw':('text_get_wmoid','Read data from database using all or part of TTAAII'),
         '-rs':('text_get_site','Read data from database using all or part of CCCC'),
         '-rt':('text_get_time','Read data from database using all or part of date/time group ddhhmm'),
         '-ri':('text_get_prodid','Read data from database using all or part of AWIPS ID NNNXXX'),
         '-rb':('text_get_bbb','Read data from database using bbb'),
         '-rh':('text_get_hh','Read data from database with special headers'),
         
         '-w':('text_put','Write to the text database'),
         'write':('text_put','Write to the text database'),
         
         '-t':('text_get_latest_def','Display write time of latest version(s)'),
         '-tU':('text_get_latest_unx','Display write time of latest version(s) - Unix formatted'),
         '-A':('text_get_all_def','Display all times for one productID'),
         '-AU':('text_get_all_unx','Display all times for one productID - Unix format'),
         
         '-va':('versions_put','Change the number of versions to keep in the database'),
         '-vr':('versions_get','Display the number of versions to keep in the database'),
         
         '-sa':('state_put','Add another ID to the SS.NNN lookup list'),
         '-sd':('state_del','Delete an ID from the SS.NNN lookup list'),
         '-sr':('state_get','Display current list for state in SS.NNN lookup list'),
         
         '-la':('pil_put','Add the product ID and the script path to the watchWarn table for sending to pil'),
         '-al':('pil_put','Add the product ID and the script path to the watchWarn table for sending to pil'),         
         '-ld':('pil_del','Delete the product ID and the script path to the watchWarn table for sending to pil'),
         '-dl':('pil_del','Delete the product ID and the script path to the watchWarn table for sending to pil'),
         '-lr':('pil_get','Retrieve all scripts for a product ID'),
         '-rl':('pil_get','Retrieve all scripts for a product ID'),
         
         '-c':('afos_cmds','Reviews AFOS commands'),
         '-k':('awips_cmds','Reviews AWIPS commands'),
         
         '-n':('site_node', 'Display the current site')
         }

# list defining the commands that have products associated
products = ['text_put']

# list defining the pil (script runner) based commands
ldadcmds = ['pil_put','pil_del','pil_get']
ldadput = 'pil_put'
ldaddel = 'pil_del'
convldad = {'pil_put':'-o add -t pil -r pil -p %s -f %s -c %s',
            'pil_del':'-o delete -t pil -r pil -p %s -f %s',
            'pil_get':'-o read -t pil -r pil -p %s'}

# list trigger based commands
triggercmds = ['trig_add', 'trig_del']
convtrig = {'pil_put':'-o add -p %s -f %s'}

# list defining the commands that need to have data combined
mergeData = ['text_get_prod',
             'text_get_info',
             'text_getAwips_prod']

# list defining the commands that have multiple products.
multiData = ['text_get_latest_def',
             'text_get_latest_unx']

# list defining the commands that may be joined in a single textdb session.
mayJoin = ['text_get_wmoid',
           'text_get_site',
           'text_get_time',
           'text_get_prodid',
           'text_get_bbb',
           'text_get_hh']

# Dictionary that translates command line into Message header properties.
# The basic structure of entries is
#       'command':[(key,value),...,(key,value)]
# Each key/value pair is a two element tuple containing a key (string) and a value.
# The first tuple for each command is the 'ARGS' tuple, which defines the number of
# command line values the command requires.
#
# The standard keys are
#       ARGS     : the number of command line arguments required for the request
#       VIEW     : the view used by TextDBSrv to process the request [state,warn,???]
#       OP       : the operation used by TextDBSrv to process the request
#       SUBOP    : the sub-operation used by TextDBSrv to process the request (if needed)
# other may be included depending on the specific command. A numeric value specifies the
# position in the corresponding argument list for the command containing the value. An
# 'ARGS' value of -1 indicates the command may have a variable number of arguments (1 or more).
MSG_ARGS = 0      # position of the arguments count tuple
MSG_START = 1     # position of first tuple used to generate message
MSG_KEY = 0       # index into tuple for tuple name
MSG_VALUE=1       # index into tuple for tuple value
MSG_VAR_ARGS=-1   # argument count value indicating variable arguments
message = {'text_get_prod':      [('ARGS',1),  ('VIEW','text'),      ('OP','GET'),    ('SUBOP','PROD'),   ('AFOSCMD',0)                     ],
           'text_get_info':      [('ARGS',1),  ('VIEW','text'),      ('OP','GET'),    ('SUBOP','INFO'),   ('AFOSCMD',0)                     ],
           'text_getAwips_prod': [('ARGS',1),  ('VIEW','text'),      ('OP','GET'),    ('SUBOP','PROD'),   ('AWIPSCMD',0)                    ],
           'text_getAwips_info': [('ARGS',1),  ('VIEW','text'),      ('OP','GET'),    ('SUBOP','INFO'),   ('AWIPSCMD',0)                    ],
           
           'text_get_wmoid':     [('ARGS',1),  ('VIEW','text'),      ('OP','GET'),    ('SUBOP','JOIN'),   ('WMOID',0)                       ],
           'text_get_site':      [('ARGS',1),  ('VIEW','text'),      ('OP','GET'),    ('SUBOP','JOIN'),   ('SITE',0)                        ],
           'text_get_time':      [('ARGS',1),  ('VIEW','text'),      ('OP','GET'),    ('SUBOP','JOIN'),   ('HDRTIME',0)                     ],
           'text_get_prodid':    [('ARGS',1),  ('VIEW','text'),      ('OP','GET'),    ('SUBOP','JOIN'),   ('NNNXXX',0)                      ],
           'text_get_bbb':       [('ARGS',1),  ('VIEW','text'),      ('OP','GET'),    ('SUBOP','JOIN'),   ('BBB',0)                         ],
           'text_get_hh':        [('ARGS',1),  ('VIEW','text'),      ('OP','GET'),    ('SUBOP','JOIN'),   ('FHOUR',0)                       ],
           
           'text_get_latest_def':[('ARGS',-1), ('VIEW','text'),      ('OP','GET'),    ('SUBOP','LATEST'), ('FORMAT','DEFAULT'), ('PRODID',0)],
           'text_get_latest_unx':[('ARGS',-1), ('VIEW','text'),      ('OP','GET'),    ('SUBOP','LATEST'), ('FORMAT','UNIX'),    ('PRODID',0)],
           'text_get_all_def':   [('ARGS',1),  ('VIEW','text'),      ('OP','GET'),    ('SUBOP','ALL'),    ('FORMAT','DEFAULT'), ('PRODID',0)],
           'text_get_all_unx':   [('ARGS',1),  ('VIEW','text'),      ('OP','GET'),    ('SUBOP','ALL'),    ('FORMAT','UNIX'),    ('PRODID',0)],
           
           'text_put':           [('ARGS',1),  ('VIEW','text'),      ('OP','PUT'),    ('PRODID',0),       ('product',1)                     ],
           
           'versions_get':       [('ARGS',1),  ('VIEW','versions'),  ('OP','GET'),    ('PRODID',0),                                         ],
           'versions_put':       [('ARGS',2),  ('VIEW','versions'),  ('OP','PUT'),    ('PRODID',0),       ('VERSION',1)                     ],
           
           'pil_put':           [('ARGS',2),  ('VIEW','warn'),      ('OP','PUT'),    ('PRODID',0),       ('SCRIPT',1)                      ],
           'pil_get':           [('ARGS',1),  ('VIEW','warn'),      ('OP','GET'),    ('PRODID',0)                                          ],
           'pil_del':           [('ARGS',2),  ('VIEW','warn'),      ('OP','DELETE'), ('PRODID',0),       ('SCRIPT',1)                      ],
           
           'state_put':          [('ARGS',3),  ('VIEW','state'),     ('OP','PUT'),    ('STATE',0),        ('XXX',1),             ('CCC',2)  ],
           'state_get':          [('ARGS',1),  ('VIEW','state'),     ('OP','GET'),    ('STATE',0)                                           ],
           'state_del':          [('ARGS',3),  ('VIEW','state'),     ('OP','DELETE'), ('STATE',0),        ('XXX',1),             ('CCC',2)  ],
           
           'afos_cmds':          [('ARGS',0)                                                                                            ],
           'awips_cmds':         [('ARGS',0)                                                                                            ],
           
           'trig_add':           [('ARGS',2), ('VIEW','warn'),       ('OP','PUT'),    ('PRODID',0),       ('SCRIPT',1)                      ],
           'trig_del':           [('ARGS',1), ('VIEW','warn'),       ('OP','DELETE'), ('PRODID',0)                                          ],
           'site_node':          [('ARGS',-2), ('VIEW','text'),       ('OP','GET'),    ('SUBOP','INFO'),   ('SITE',0)                                                                                           ]
           }

# Usage message to print when help is requested. Displayed when text database CLI tool is
# run with either no valid flags or a '-h' flag.
USAGE_MESSAGE = \
"""
usage: textdb -r AFOSCmd                 Does a standard afos read
       textdb -rk AWIPSCmd               Does a standard awips read

       textdb -rw wmoid -rs site -ri NNNXXX -rh HH -rt ddhhmm -rb bbb
           Does AWIPS read on one or more fields
           ***  Note that not all combinations are valid  ***
           
       textdb -w productID < product     Writes a product to database
       textdb -t productID(s)            Gets create time(s) for product(s)
       textdb -tU productID(s)           Gets (UNIX) create time(s) for product(s)
       textdb -A ProductID               Gets all create times (GMT) for a product
       textdb -AU ProductID              Gets all create times (UNIX) for a product
       textdb -rd AFOSCmd                Displays latest products with product count and product lengths
       textdb -rkd AWIPSCmd              Displays latest AWIPS products with product count and product lengths
       textdb -v ProductID [N]           Read[Changes] a product versions-to-keep [to N]
       textdb -s -a SS XXX CCC           Adds XXX to state [SS] listing
       textdb -s -d SS XXX CCC           Deletes XXX from state [SS]
       textdb -s -r SS                   Displays XXX list for state [SS]
       textdb -pil -a productID script   Adds a AWIPS or AFOS PIL watch warning product with script pathname
       textdb -pil -d productID script   Deletes a AWIPS or AFOS PIL watch warning product with script pathname
       textdb -c                         Reviews AFOS commands  
       textdb -k                         Reviews AWIPS commands
       textdb -n [site]                  Gets current node (CCC) site for running edex
"""
# AFOS commands summary. Displayed when text database CLI tool is run with
# the '-c' flag.
AFOS_CMDS = \
"""
AFOSCmd: ALL:CCCNNNXXX  Read all product versions of CCCNNNXXX
         ALL:NNNXXX     Read all product versions of NNNXXX for local node CCC
         A:HH CCCNNNXXX Read all versions of product CCCNNNXXX from last HH hours
         A:HH 000NNNXXX Read all versions of product NNNXXX at all nodes from last HH hours
         A:HH CCCNNN    Read latest version of all products CCCNNN for all sites XXXs from last HH hours
         A:HH NNNXXX    Read latest version of all products NNNXXX for local node CCC from last HH hours
         A:HH NNN       Read latest version of all products NNN for local node CCC and all sites XXXs from last HH hours
         A:CCCNNN       Read current-hour version of all products CCCNNN for all sites XXXs
         A:NNN          Read current-hour version of all products for local node CCC and all sites, XXXs
        -N:CCCNNNXXX    Read Nth previous version of product for CCCNNNXXX
        -N:NNNXXX       Read Nth previous version of product NNNXXX for local node CCC
        -:CCCNNNXXX     Read previous version of latest product for CCCNNNXXX
        -:NNNXXX        Read previous version of latest product NNNXXX for local node CCC
         SS.NNN         Read latest version of all products for NNN and state SS
         NNNXXX         Read latest version of all products NNNXXX for local node CCC
         CCCNNN000      Read latest version of all products for all sites, XXXs
         NNN000         Read latest version of all products NNN for local node CCC and all sites XXXs
         CCCNNNXXX      Read latest version of product for CCCNNNXXX
"""
# AWIPS commands summary. Displayed when text database CLI tool is run with
# the '-k' flag.
AWIPS_CMDS = \
"""
AWIPSCmd: ALL:CCCCNNNXXX  Read all product versions of NNNXXX for site CCCC
          ALL:NNNXXX      Read all product versions of NNNXXX for local site CCCC
          A:HH CCCCNNNXXX Read all versions of product NNNXXX for site CCCC from last HH hours
          A:HH 0000NNNXXX Read all versions of product NNNXXX for all sites from last HH hours
          A:HH CCCCNNN    Read latest version of all products for site CCCC with NNN and all XXXs from last HH hours
          A:HH NNNXXX     Read latest version of all products NNNXXX for local site CCCC from last HH hours
          A:HH NNN        Read latest version of all products NNN for local site CCCC and all XXXs from last HH hours
          A:CCCCNNN       Read current-hour version of all products for sote CCCC and product NNN for all XXXs
          A:NNN           Read current-hour version of all products for local site where XXX is any value
         -N:CCCCNNNXXX    Read Nth previous version of product for site CCCC and product NNNXXX
         -:CCCCNNNXXX     Read previous version of latest product NNNXXX for site CCCC
         -:NNNXXX         Read previous version of latest product NNNXXX for local site CCCC
         -N:NNNXXX        Read Nth previous version of product NNNXXX for local site CCCC
          SS.NNN          Read latest version of all products for NNN and state SS
          NNNXXX          Read latest version of product NNNXXX for local site
          CCCCNNN000      Read latest version of products for node CCCC where XXX is any value
          NNN000          Read latest version of all products NNN for local site where XXX is any value
          CCCCNNNXXX      Read latest version of product NNNXXX for site CCCC
"""

