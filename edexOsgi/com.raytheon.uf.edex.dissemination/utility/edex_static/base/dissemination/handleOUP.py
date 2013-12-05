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
##

#
# Derived from port of handleOUP.pl, but diverged to support single path
# of dissemination.  Assigns a priority to the product, and attempts to send
# it to the message handling system if it's not in the include lists.   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/28/09                      njensen       Initial Creation.
#    12/09/09        DR3778        M. Huang      Add acknowledgment handling
#    09/05/11        DR9602        M. Huang      Fix acknowledgment handling error
#    04/13/12        DR 10388      D. Friedman   Correct acknowledgment handling
#    08/17/12        DR 15304      D. Friedman   Use unique output file names
#    10/12/12        DR 15418      D. Friedman   Use unique attachment file names
#    11/20/13        DR 16777      D. Friedman   Add a test mode.
# 
#

import time, os, os.path, sys, subprocess, select, errno
import logging, UFStatusHandler
from com.raytheon.uf.common.dissemination import OUPResponse
_Logger = logging.getLogger("HandleOUP")
_Logger.addHandler(UFStatusHandler.UFStatusHandler("com.raytheon.uf.edex.dissemination", "HandleOUP", level=logging.INFO))
_Logger.setLevel(logging.INFO)

DB_SUCCESS = 0;
DB_FAILURE = 1;
DB_TIMEOUT = 2;
DB_DUPLICATE = 4;

ACTION_CODES = {}
from com.raytheon.uf.common.localization import PathManagerFactory
from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType, LocalizationContext_LocalizationLevel as LocalizationLevel
pathMgr = PathManagerFactory.getPathManager()
ctx = pathMgr.getContext(LocalizationType.valueOf('EDEX_STATIC'), LocalizationLevel.valueOf('BASE'))
DPATH = pathMgr.getFile(ctx, 'dissemination').getPath()
f = open(DPATH + '/rcv_action2codes.txt')
for line in f:
    codeSplit = line.split()
    ACTION_CODES[codeSplit[0]] = codeSplit[1]
f.close()

from com.raytheon.uf.edex.core.props import PropertiesFactory
env = PropertiesFactory.getInstance().getEnvProperties()
dataDir = env.getEnvValue("DEFAULTDATADIR")
OUT_DIR = dataDir + 'outgoing'
if not os.path.isdir(OUT_DIR):
    os.mkdir(OUT_DIR)
INGEST_DIR = dataDir + 'manual'
INGEST_ROUTE = 'handleoupFilePush'
SITE_ID = env.getEnvValue('SITENAME')

def process(oup, afosID, resp, ackMgr = None, test = False):
    _Logger.info("handleOUP.py received " + str(oup.getFilename()))
    wmoTypeString = ""
    userDateTimeStamp = ""
    msg = ''
    
    # WMO message type, aka bbb
    if oup.getWmoType():
        wmoTypeString = oup.getWmoType().upper()
    
    # address
    address = oup.getAddress()
    if address == 'DEF' or address == 'ALL':
        address = 'DEFAULTNCF,NWWSUP'
    elif address is None:
        address = 'DEFAULTNCF,NWWSUP'
    
    # source, possibly None
    source = oup.getSource()  
    
     # time stamp DDHHMM
    if oup.getUserDateTimeStamp():
        userDateTimeStamp = oup.getUserDateTimeStamp().upper()
        if len(userDateTimeStamp) != 6:
            msg = "Error: User date time stamp is wrong length\n"
            _Logger.error("User date time stamp is wrong length")
            resp.setMessage(msg)
            return
    
    #----------
    # Initialize the product identifier 
    #----------
    awipsWanPil = oup.getAwipsWanPil()
    _Logger.debug('awipsWanPil = ' + awipsWanPil)        
    
    #----------
    # Extract the category ( NNN of CCCCNNNXXX ) from the awips ID 
    #----------
    prodCategory = getCategory(awipsWanPil)
    _Logger.debug("Product Category = " + prodCategory)

    #----------
    # Determine the transmission priority for WAN distribution 
    #---------- 
    priority = getPriority(prodCategory)
    oup.setPriority(priority)
    _Logger.debug('Priority = ' + str(priority))

    #----------
    #  Retrieve the contents of the product
    #----------
    contents = oup.getProductText()
    productId = contents.split('\n')[0].strip()
    
    #----------
    # Locally store OUP in text database and archive
    #----------    
    awipsPathname = createTargetFile(contents, 
            OUT_DIR + '/' + oup.getFilename())
    if not awipsPathname:
        _Logger.debug('Unable to store product to text database:')
        storageCompleted = DB_FAILURE
        msg = 'Product ' + awipsWanPil + ' failed to be ingested and archived.\n'
        _Logger.debug(msg)
        resp.setMessage(msg)
        return
    elif not test:
        try:
            from com.raytheon.uf.edex.plugin.manualIngest import MessageGenerator
            if MessageGenerator.getInstance().sendFileToIngest(awipsPathname, INGEST_ROUTE):
               msg = 'Product ' + awipsWanPil + ' successfully ingested and archived locally.\n'
               resp.setSendLocalSuccess(True)
               _Logger.info(msg)
            else:
               msg = 'Product ' + awipsWanPil + ' failed to be ingested and archived.\n'
               resp.setMessage(msg)
               return
        except Exception, e:
            msg = 'Product ' + awipsWanPil + ' failed to be ingested and archived properly. Reason:\n' + str(e)
            resp.setMessage(msg)
            return
    
    attachedFilename = oup.getAttachedFilename()
    attachedFile = oup.getAttachedFile()
    if attachedFilename and attachedFile:
        # spaces will screw up the command line string
        attachedFilename = attachedFilename.replace(" ", "")
        # dealing with a java byte[] so write it out with java
        from java.io import File, FileOutputStream
        attachedFilename = createTargetFile("", OUT_DIR + '/' + attachedFilename)
        f = File(attachedFilename)
        fos = FileOutputStream(f)
        fos.write(attachedFile)
        fos.flush()
        fos.close()

    if test:
        try:
            os.remove(awipsPathname)
        except EnvironmentError:
            pass # ignore
        if attachedFilename:
            try:
                os.remove(attachedFilename)
            except EnvironmentError:
                pass # ignore

        resp.setSendLocalSuccess(True)
        resp.setSendWANSuccess(True)
        return

    messageIdToAcknowledge = None
    #----------
    # Check if product should be distributed over WAN via NCF
    #----------
    wmoID = contents[0:6]
    splitAddr = address.split(',')
    for addr in splitAddr:
        if addr != '000': # 000 is local only
            _Logger.info("Addressee is " + addr)
            #----------
            # Check if product should be sent to the NWWS for uplink
            #----------
            if (addr.find('NWWSUP') > -1):                
                if isNWWSProduct(awipsWanPil, afosID, wmoID, SITE_ID):
                    #----------
                    # Send OUP to its designated NWWS primary and backup sites for up-link   
                    #----------
                    code = "NWWS_UPLINK"
                    if source and source == 'TextWS':
                        code = "42"
                    sendResult = sendWANMsg(productId, awipsPathname, addr, code,
                                  userDateTimeStamp, priority, wmoTypeString, source, resp, afosID, attachedFilename)
                    if not sendResult:
                        #failure of some kind so return
                        return     
                else:
                    _Logger.debug("Product is not an NWWS product.  Not sending product " +
                                  'over NWWS up-link.')
            else:
                if isLegalWANProduct(awipsWanPil, afosID, wmoID, SITE_ID):        
                    #----------
                    # Send OUP to the NCF
                    #----------
                    code = "0"
                    if source and source == 'TextWS':
                        if (prodCategory == 'ADR' or prodCategory == 'ADM' or prodCategory == 'ADA') and \
                                attachedFilename:
                            code = "7"
                        else:
                            code = "4"
                    sendResult = sendWANMsg(productId, awipsPathname, addr, code, userDateTimeStamp,
                                  priority, wmoTypeString, source, resp, afosID, attachedFilename)
                    if not sendResult:
                        #failure of some kind so return
                        return
                    # Copy this now as the values may change in another loop iteration
                    if resp.getNeedAcknowledgment() and messageIdToAcknowledge is None:
                        messageIdToAcknowledge = resp.getMessageId() 
                else:
                    _Logger.info("Product is not authorized for distribution.")
                    _Logger.info("Not sending product to NCF.")
                    msg = "Warning: Product is not authorized for distribution.\n"
                    resp.setMessage(msg)
                    return

    if messageIdToAcknowledge:
        resp.setNeedAcknowledgment(True)
        resp.setMessageId(messageIdToAcknowledge)
        if ackMgr != None:
            _Logger.info("Waiting for acknowledgment of " + messageIdToAcknowledge)
            ackMgr.waitAck(messageIdToAcknowledge, address, resp, 
                           afosID + " " + userDateTimeStamp)
            _Logger.info("Finished waiting for acknowledgment of %s: %s" %
                         (messageIdToAcknowledge, resp.isAcknowledged() and 
                          "ACK" or resp.getMessage()))
            if not resp.isAcknowledged():
                # Send ITO alarm
                ito_err = None
                try:
                    ec = subprocess.call(['/opt/OV/bin/OpC/opcmsg', 'application=MHS', 'object=MHS',
                                     'msg_text=%s (msgid %s)' % (resp.getMessage(), messageIdToAcknowledge),
                                     'severity=Critical', 'msg_grp=AWIPS'])
                    if ec != 0:
                        ito_err = 'exit code = ' + str(ec)
                except:
                    ito_err = str(sys.exc_info()[1])
                if ito_err is not None:
                    _Logger.error("Error sending ITO alarm: " + ito_err)
        else:
            _Logger.error("Acknowledgment requirement, but ackMgr is None")
    
    _Logger.debug('Script done....')
    return
            

#---getCategory()--------------------------------------------------------------#
# 
#   Purpose:
#       Determines the product category from the AWIPS identifier.
#
#   Parameters: 
#       AWIPS product identifier (CCCCNNNXXX)
# 
#   Returns:                                     
#       3-letter product category (NNN of CCCCNNNXXX)
#
#   Implementation:
# 
#------------------------------------------------------------------------------#
def getCategory(awipsID):
    _Logger.debug("getCategory():")    
    return awipsID[4:7]


#---getPriority()--------------------------------------------------------------#
#
#   Purpose:
#       Returns the priority level of the product based on its category.
#
#   Parameters: 
#       3 letter product category (NNN)
# 
#   Returns:                                     
#       Priority level [0,1,2] where 2 = highest 
#
#   Implementation:
#       Searches awipsPriorities.txt using the product category as the key. 
#       If the file does not contain the specified category entry, the lowest 
#       priority level is assumed.
#       Exits program if file cannot be opened.
# 
#------------------------------------------------------------------------------#
def getPriority(nnn):
    _Logger.debug('getPriority():')
    
    priority = "0"
    pfile = open(DPATH + '/awipsPriorities.txt', 'r')
    for line in pfile:
        if nnn == line[0:3]:
            _Logger.debug(line)
            priority = line[4:].strip()
            break
    pfile.close()
    
    priority = int(priority) 
    return priority

#---isLegalWANProduct()------------------------------------------------------#
#
#   Purpose:
#       Determines whether the product is a valid NWWS product.
#
#   Parameters:
#       AWIPS identifier (CCCCNNNXXX)
#       AFOS  identifier (CCCNNNXXX)
#       WMO   identifier (TTAAII)
#
#   Returns:
#       1 (TRUE)/ 0 (FALSE)
#
#   Implementation:
#       Reads the site-specific WAN exclusionary list which contains a
#       list of product ids representing products which are not meant for
#       distribution over WAN via NCF.  The AWIPS id, the AFOS id,
#       and the WMO id, are acceptable representations of the product id.
#
#       If the exclusionary file either does not exist, is empty, or cannot
#       be read, then the product will be distributed.
#   
#       Program exits if the site id environment variable (FXA_LOCAL_SITE)
#       is undefined.
#
#------------------------------------------------------------------------------#
def isLegalWANProduct(myAwipsId, myAfosId, myWmoid, siteID):
    _Logger.debug('isLegalWANProduct():')
    
    # Read the WAN exclusionary file
    filename = DPATH + 'WAN_exclude_' + siteID + '.txt'
    if os.path.isfile(filename):
        wanExcludeFile = open(filename, 'r')
        for line in wanExcludeFile:
            if not line.startswith('#'):
                productId = line.strip()
                if productId == myAwipdsId or productId == myAfosId or productId == myWmoId:
                    _Logger.info('Product found in WAN exclude list as ' + productId)
                    wanExcludeFile.close()
                    return False
        # Otherwise, product did not appear on the exclude list and therefore,
        # product is meant for distribution
        _Logger.info(myAwipsId + ' is a legal WAN product.')
        wanExcludeFile.close()
        return True        
    else:
        _Logger.info(filename + ' does not exist or is empty.  Sending ' +
                      'product over WAN.' )
        return True


#---sendWANMsg()---------------------------------------------------------------#
#
#   Purpose:
#       Distributes an OUP to a specified receiving site over the WAN.
#
#   Parameters: 
#       receiving site
#       handling action to take on product at the receiving site
#       additional option string
# 
#   Returns:                                     
#       1 (TRUE) = successful message submission
#       0 (FALSE) = unsuccessful message submission
#
#   Implementation:
#       Uses system() function to execute the distributeProduct program.
#
#------------------------------------------------------------------------------#
def sendWANMsg(productId, prodPathName, receivingSite, handling,
               userDateTimeStamp, priority, wmoTypeString, source, resp, subject=None, attachedFilename=None):
   # _Logger.debug('sendWANMsg():')
    _Logger.info('sendWANMsg ' + str(prodPathName) + ' addr=' + str(receivingSite) + \
                 ' code=' +str(handling) + ' source=' + str(source))
    
    try:
        code = int(handling)
    except:   
        code = int(ACTION_CODES[handling])    
    # set acknowledgment from receiver if message is high priority and is from TextWS
    # resp = OUPResponse()
            
        
    from com.raytheon.messaging.mhs import MhsMessage, MhsMessagePriority, MhsSubmitException
    mhsMsg = MhsMessage(code)

    if subject:
        mhsMsg.setSubject(subject)            
    
    if attachedFilename:
        mhsMsg.addEnclosure(attachedFilename)

    mhsMsg.setProductId(productId)
    #mhsMsg.setBodyFile(prodPathName)
    mhsMsg.addEnclosure(prodPathName)
    if priority == 0:
        jpriority = MhsMessagePriority.Default
    elif priority == 1:
        jpriority = MhsMessagePriority.Medium
    elif priority == 2:
        jpriority = MhsMessagePriority.High
    mhsMsg.setPriority(jpriority)  
    
    if priority == 2 and source == "TextWS":
        resp.setNeedAcknowledgment(True)
        mhsMsg.addAckAddressee(receivingSite)
        mhsMsg.setTimeoutTime(300)
    else:
        # No need to get acknowledgment from receiver
        resp.setNeedAcknowledgment(False)
        mhsMsg.addAddressee(receivingSite)

    _Logger.info("Calling mhsMsg.send()")
    result = mhsMsg.send()
    
    if not result:
        result = "Error sending product " + productId + " to " + receivingSite + ".  Check server logs for more detail.\n"
        _Logger.error(result)
        resp.setSendWANSuccess(False)
        resp.setMessage(result)
        return False
    else:
        resp.setSendWANSuccess(True)
        if resp.getNeedAcknowledgment():
            resp.setMessageId(result)

        _Logger.info("Successful send of " + str(result))
    return True
    

#---isNWWSProduct()------------------------------------------------------------#
#
#   Purpose:
#       Determines whether the product is a valid NWWS product.
#
#   Parameters:
#       AWIPS identifier (CCCCNNNXXX)
#       AFOS  identifier (CCCNNNXXX)
#       WMO   identifier (TTAAII)
#
#   Returns:
#       1 (TRUE)/ 0 (FALSE)
#
#   Implementation:
#       Reads the site-specific NWWS exclusionary list which contains a
#       list of product ids representing products which are not meant for
#       distribution over the NWWS up-link.  The AWIPS id, the AFOS id,
#       and the WMO id, are acceptable representations of the product id.
#
#       If the exclusionary file either does not exist, is empty, or cannot
#       be read, then the product will be uplink.
#   
#       Program exits if the site id environment variable (FXA_LOCAL_SITE)
#       is undefined.
#
#       This module addresses DR 5191.
#   
#------------------------------------------------------------------------------#
def isNWWSProduct(myAwipsId, myAfosId, myWmoId, siteID):
    _Logger.debug('isNWWSProduct():\n')
    
    # Read the NWWS exclusionary file
    filename = DPATH + 'NWWS_exclude_' + siteID + '.txt'
    if os.path.isfile(filename):
        nwwsExcludeFile = open(filename, 'r')
        for line in nwwsExcludeFile:
            # If entry is found, then product should not be distributed
            # over the NWWS uplink
            if not line.startswith('#'): # skips comment lines
                productId = line.strip()
                if productId == myAwipdsId or productId == myAfosId or productId == myWmoId:
                    _Logger.info('Product found in NWWS exclude list as ' + productId)
                    nwwsExcludeFile.close()
                    return False
        # Otherwise, product did not appear on the exclude list and therefore,
        # product is meant for distribution
        _Logger.info(myAwipsId + ' is an NWWS product.')
        nwwsExcludeFile.close()
        return True        
    else:
        _Logger.info(filename + ' does not exist or is empty.  Sending ' +
                      'product over NWWS uplink.' )
        return True

    


#---createTargetFile()---------------------------------------------------------#
# 
#   Purpose:
#       Creates a product file in the named target directory with a selected
#       communications header for identification.
#
#   Parameters: 
#       communications header 
#       product contents
#       target product pathname (Output)
# 
#   Returns:
#       The output path (which may differ from targetPathname)        
#
#   Implementation:
#
#------------------------------------------------------------------------------#
def createTargetFile(fileData, targetPathname):
    _Logger.debug('createTargetFile():')
    _Logger.debug('target product pathname = ' + targetPathname)

    pathToUse = targetPathname
    i = 0
    while True:
        try:
            fd = os.open(pathToUse, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0666)
        except OSError, e:
            if e.errno == errno.EEXIST:
                i += 1
                pathToUse = targetPathname + '.' + str(i)
                continue
            raise e
        else:
            break
   
    if i > 0:
       _Logger.info('Renamed target file to ' + pathToUse)
    
    outFile = os.fdopen(fd, 'w')
    outFile.write(fileData)
    outFile.flush()
    outFile.close()
    return pathToUse
    
