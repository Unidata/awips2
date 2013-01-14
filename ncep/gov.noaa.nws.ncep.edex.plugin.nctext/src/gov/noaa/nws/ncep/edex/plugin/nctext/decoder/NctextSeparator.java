/**
 * 
 * NctextSeparator 
 * 
 * This java class performs text data separating function
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 10/23/2009		TBD		Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.nctext.decoder;

import gov.noaa.nws.ncep.edex.plugin.nctext.common.NctextRecord;
import gov.noaa.nws.ncep.edex.plugin.nctext.common.dao.NctextInputFileTypeDao;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.DataFormatException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

public class NctextSeparator extends AbstractRecordSeparator {

    Log logger = LogFactory.getLog(getClass());

    public static final String WMO_HEADER = "(([A-Z]{4})(\\d{0,2}|[A-Z]{0,2})) ([A-Z]{4}) (\\d{6})[^\\r\\n]*[\\r\\n]+";

    private static final int WMOIDGROUP_NUMBER = 1;

    private static final int CCCCGROUP_NUMBER = 4;

    private static final int YYGGggGROUP_NUMBER = 5;

    public static final String AWIPS_ID = "([A-Z0-9]{4,6})";

    public static final String BBBID = "[A-Z]{3}";

    public static final String R_TYPE_SEPARATOR = "[A-Z]{3,4}+//";

    public static final String R_TYPE_SEPARATOR2 = "00///";

    public static final String R_TYPE_SEPARATOR3 = "[A-Z]{3,4}";
    
    public static String [][] gdStnGp= {{"PWM","BGR","CAR","CON","AFA"},{"ALB","BOS","PHL","BTV","LGA","IPT"},{"DCA","RDU","ILM","ORF","HAT"},
    {"CAE","MIA","TLH","SAV","LAL"},{"BUF","CLE","CRW","PIT","DAY","IND"},{"STL","MEM","TYS","SDF","BNA","ATL"},{"BHM","JAN","SHV","MOB","MSY","LIT"},
    {"DTW","MKE","INL","SSM","MSP","ORD"},{"DSM","DDC","LBF","TOP","OMA","BFF"},{"OKC","SAT","BRO","DFW","IAH","DRT"},{"BIS","RAP","BIL","FSD","GTF","MSO"},
    {"LBB","ABQ","DEN","ELP","PHX","CYS"},{"SEA","PDX","BOI","GEG","MFR","PIH"},{"SFO","LAX","SLC","FAT","RNO","CDC"},{"YQB","YOW","YYB","YQT","YMW","YLH"},
    {"YWG","YQR","YYC","YQD","YPA","YEG"},{"MCD","YXC","YVR","YRV","YCG","YXS"}, {"X68","EDW","LWS","UCC","BTNM","LGIN"}, {"SYR","ROA","ZZV","AOO","AVL"},
    {"SGF","GRB","GJT","JKL","MLI","FAR","LND"},{"MAF","LCH","TUL","DHT","DHN","DAB"},{"GGW","RDD","LAS","PDT","EKO","FLG"},
    {"AKN","CZF","EDF","EHM","SVW","TLJ"},{"EIL","GAL","LUR","TNC","UTO"},{"ANC","ANN","CDV","JNU","ORT","YAK"},{"ADQ","BET","CDB","MCG","SNP"},
    {"BRW","BTI","BTT","FAI","OME","OTZ"},{"TJSJ","TJMZ","TJPS","TJBQ","TJGU","TJAD"},{"TIST","TISX","TNCM","TKPK","TJNR","TISJ"},{"PHNL","PHLI","PHOG","PHTO","HIB1","HIB4"}};

    public static final byte ASCII_RS = 0x1E; // record separator "^^"
    public static final byte ASCII_SP = 0x20; // SPACE
    
    

    // private static final int DTGROUP_SIZE = 6;
    private String cccc = null;

    private String YYGGgg = null;

    private static final int YYYYMMDD_LENGTH = 8; /*
                                                   * 8 bytes long for
                                                   * year/month/day in a ingest
                                                   * file name
                                                   */

    private static final int MIN_AWIPS_HDR_LENGTH = 25;

    private String BBBIndicator;

    private static final int BBB_SIZE = 3;

    private int messageDataStart = 0; /*
                                       * start of text record data after
                                       * WMOHeader and AWIPSId
                                       */

    private int recordStart = 0; /* start of text record */

    // private boolean adjustBuffer= false;
    private enum ParseError {
        NO_CTLA_TO_END, NO_CTLA_IN_BULLETIN, NO_CTLC_TO_END, NO_AWIPS_HDR, NO_AWIPS_HDR_IN_BULLETIN, GENERAL_ERR, NO_ERR
    }

    private ParseError parseErr;

    private List<NctextRecord> reports = null;

    // WMOHeader wmoHeader = null;

    String traceId = null; /* ingest file name */

    private int currentReport = -1;

    // final TextDBStaticData staticData;

    private String awipsId;

    private String WMOId = null; // TTAAii

    private String ingestFileExt = null; // ingest file extension has
                                         // information of product type

    private NctextInputFileTypeDao nctextInputFileTypeDao;

    private static final String ERR_LOG_FOLDER = "../logs/nctext_log";

    private static final String ERR_LOG_FOLDER_PATH = ERR_LOG_FOLDER + "/";

    private File errLogFolder = null;

    private static int logNum = 1;

    /**
     * 
     * @param traceId
     *            : most case would be ingest file name
     */
    public NctextSeparator(String traceId) {
        this.traceId = traceId;
        nctextInputFileTypeDao = new NctextInputFileTypeDao();
        int ind = traceId.indexOf('.');
        if (ind > 0)
            ingestFileExt = traceId.substring(ind + 1);// use ingest file name
                                                       // extension as product
                                                       // type
        else
            ingestFileExt = "NA";
    }

    /**
     * 
     */
    @Override
    public NctextRecord next() {
        NctextRecord data = null;
        if (hasNext()) {
            data = reports.get(currentReport++);
        }
        return data;
    }

    /**
     * Is there another record available?
     * 
     * @return Is there another record available?
     */
    @Override
    public boolean hasNext() {
        return ((reports != null) && (reports.size() > 0) && (currentReport < reports
                .size()));
    }

    public List<NctextRecord> getRecordList() {
        return reports;
    }

    private void logErrToFile(String errMsg) {

        FileOutputStream outStream = null; // declare a file output object
        PrintStream pStream = null; // declare a print stream object
        errLogFolder = new File(ERR_LOG_FOLDER);
        if (errLogFolder.exists() == false) {
            if (errLogFolder.mkdir() == false) {
                logger.info(ERR_LOG_FOLDER + " create failed!");
                return;
            } else
                logger.info(ERR_LOG_FOLDER + " created!");
        } else {
            if (logNum >= 100) {
                logger.info("Too many err log files. Stop writing log file!");
                return;
            }
        }

        try {
            outStream = new FileOutputStream(ERR_LOG_FOLDER_PATH + traceId
                    + ".errLog." + logNum);
            // Connect print stream to the output stream
            pStream = new PrintStream(outStream);

            pStream.print("\n" + errMsg);
            pStream.close();
            logNum++;
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    private synchronized boolean parseHeader(String sInputMessageData) {
        // Assume not valid until proven otherwise!
        boolean isValid = false;
        // adjustBuffer = false;
        parseErr = ParseError.GENERAL_ERR;
        BBBIndicator = "REG";
        awipsId = "NA";

        if (sInputMessageData != null) {
            // CtlA is the start of a record bulletin and Ctl-C is the end of it
            int CtlAPos = sInputMessageData
                    .indexOf(IDecoderConstants.ASCII_SOM);
            int CtlCPos = sInputMessageData
                    .indexOf(IDecoderConstants.ASCII_ETX);

            if ((CtlAPos == -1)
                    && (sInputMessageData.length() <= MIN_AWIPS_HDR_LENGTH)) {
                parseErr = ParseError.NO_ERR; // end of file
                return isValid;
            }
            if (CtlAPos == -1) {
                parseErr = ParseError.NO_CTLA_TO_END;
                return isValid;
            }
            if (CtlCPos == -1) {
                parseErr = ParseError.NO_CTLC_TO_END;
                return isValid;
            }
            if (CtlAPos > CtlCPos) {
                parseErr = ParseError.NO_CTLA_IN_BULLETIN;
                return isValid;

            }
            // parse data starting from Ctl-A
            String sMessageData = sInputMessageData.substring(CtlAPos);

            Pattern p = Pattern.compile(WMO_HEADER);
            Matcher m = p.matcher(sMessageData);
            if (m.find()) {
                if (m.start() > sMessageData
                        .indexOf(IDecoderConstants.ASCII_ETX)) {
                    // This is the case where header is found after Ctr-C, in
                    // other words, the
                    // bulletin in parsing does not have header between Ctl-A
                    // and Ctl-C. Therefore,
                    // the header found is actually belongs to next bulletin.
                    // adjustBuffer = true;
                    parseErr = ParseError.NO_AWIPS_HDR_IN_BULLETIN;
                    return isValid;
                } else {
                    messageDataStart = m.end();
                    WMOId = m.group(WMOIDGROUP_NUMBER);
                    cccc = m.group(CCCCGROUP_NUMBER);

                    YYGGgg = m.group(YYGGggGROUP_NUMBER);
                    if ((m.end() - m.end(YYGGggGROUP_NUMBER)) >= BBB_SIZE) {
                        String sBBB = sMessageData.substring(
                                m.end(YYGGggGROUP_NUMBER), m.end());
                        Pattern pBBB = Pattern.compile(BBBID);
                        Matcher mBBB = pBBB.matcher(sBBB);

                        // If there is an optional BBB;
                        if (mBBB.find()) {
                            BBBIndicator = mBBB.group();
                        }
                    }
                    // parse 2nd line to get AWIPSId nnnxxx
                    String sl = sMessageData.substring(m.end());/*
                                                                 * String from
                                                                 * 2nd line down
                                                                 * to end
                                                                 */
                    int endLineCR = sl.indexOf(IDecoderConstants.ASCII_CR);
                    int endLineLF = sl.indexOf(IDecoderConstants.ASCII_LF);
                    sl = sl.substring(0, Math.min(endLineCR, endLineLF));// String
                                                                         // of
                                                                         // 2nd
                                                                         // line
                                                                         // only
                    Pattern pAWIPS = Pattern.compile(AWIPS_ID);
                    Matcher ml = pAWIPS.matcher(sl);
                    if (ml.find()) {
                        awipsId = ml.group();
                        messageDataStart = ml.end();
                    }
                    // set Record start pointer (recordStart), remove CR, LN and
                    // Soh (Ctl-A) from the record head
                    recordStart = 0;
                    int msgleng = sMessageData.length();
                    while (msgleng > 0) {
                        if ((sMessageData.charAt(recordStart) == IDecoderConstants.ASCII_SOM)
                                || (sMessageData.charAt(recordStart) == IDecoderConstants.ASCII_LF)
                                || (sMessageData.charAt(recordStart) == IDecoderConstants.ASCII_CR)) {
                            // skip ASCII_SOM ( I think it is a typo in imported
                            // package, should be SOH, Ctl-A )
                            // also skip CR and LN
                            msgleng--;
                            recordStart++;
                        } else {
                            break;
                        }

                    }
                    isValid = true;
                }
            } else {
                parseErr = ParseError.NO_AWIPS_HDR;
            }
        }
        return isValid;
    }

    /**
     * Set the raw message data and invoke the internal message separation
     * process.
     * 
     * @param rawMessage
     *            The raw weather text message.
     */
    @Override
    public void setData(byte[] rawMessage, Headers headers) {
    	/*
        currentReport = -1;

        // Now check for some binary data types, Stop decoding, if it is binary
        String sRawMessage = new String(rawMessage);
        int pos = sRawMessage.indexOf("BUFR");
        boolean notStored = false;
        notStored = notStored || ((pos >= 0) && (pos < 20));
        pos = sRawMessage.indexOf("GRIB");
        notStored = notStored || ((pos >= 0) && (pos < 20));
        pos = sRawMessage.indexOf("JPG");
        notStored = notStored || ((pos >= 0) && (pos < 20));
        pos = sRawMessage.indexOf("PNG");
        notStored = notStored || ((pos >= 0) && (pos < 20));
        pos = sRawMessage.indexOf("GIF87");
        notStored = notStored || ((pos >= 0) && (pos < 20));

        int rawMsglength = rawMessage.length; //
                                              // * start from this, and
                                              // * decremented when each bulletin
                                              // * found
                                             //  
        int rawMsgPointer = 0;//*
                              // * start from 0 and incremented when each bulletin
                              // * found
                              // 
        int recordId = 1; ///* to record number of records with same AWIPS header 
        reports = new ArrayList<NctextRecord>();
        String sRawMsgInProcessing = null;
        int endPos;
        int rsPos, nextRsPos, curPos;
        if (!notStored) {
            String fileType = nctextInputFileTypeDao
                    .getFiletypeType(ingestFileExt);
            while (rawMsglength > 0) {
                sRawMsgInProcessing = sRawMessage.substring(rawMsgPointer);
                // Parsing record header and find a "section" A section may
                // contain more than one record for some data type, e.g. M type
                // (MOS).
                if (parseHeader(sRawMsgInProcessing)) {
                    // Start of a "section" found after a Ctl-A,
                    endPos = sRawMsgInProcessing
                            .indexOf(IDecoderConstants.ASCII_ETX); // Ctl-C
                    // set data to record
                    if (endPos > messageDataStart) {
                        // handle special type of data, e.g M,etc.. type
                        if (fileType.equals("M")) {

                            // M type data has several records in one "section".
                            // They are separated by Record Separator "^^".
                            rsPos = sRawMsgInProcessing.indexOf(ASCII_RS); // find
                                                                           // first
                                                                           // RS
                            // testCount++;
                            // if((testCount %50) == 0){
                            // System.out.println("M type data: " + testCount+
                            // " enterred! end position = "+ endPos+
                            // "1st rs pos = "+rsPos);
                            // }

                            // cumRsPos = rsPos;
                            if ((rsPos >= 0) && (rsPos < endPos)) {
                                // String sMRawMsg =
                                // sRawMsgInProcessing.substring(rsPos+1);//move
                                // string to after first RS
                                // get stnid=xxxx which is 4 chars right after
                                // RS, like this ^^XXXX
                                // String stnId = sMRawMsg.substring(0, 4);
                                String stnId = sRawMsgInProcessing.substring(
                                        rsPos + 1, rsPos + 5);
                                // nextRsPos = sMRawMsg.indexOf(ASCII_RS); //2nd
                                // RS
                                curPos = rsPos + 1;
                                nextRsPos = sRawMsgInProcessing.indexOf(
                                        ASCII_RS, curPos); // 2nd RS
                                // cumRsPos = cumRsPos + nextRsPos;
                                int testCount1 = 0;
                                while ((nextRsPos >= 0)
                                        && (nextRsPos < endPos)) {
                                    // find record(s) within the section, store
                                    // it
                                    testCount1++;
                                    // if((testCount1 %50) == 1)
                                    // System.out.println(" number RS = "+
                                    // testCount);
                                    // System.out.println("cur pos = "+curPos+
                                    // " next rs pos = "+ nextRsPos);
                                    NctextRecord nctextrecord = new NctextRecord();
                                    setNctextRecord(nctextrecord);
                                    // nctextrecord.setRawRecord(sMRawMsg.substring(0,
                                    // nextRsPos-1));
                                    nctextrecord
                                            .setRawRecord(sRawMsgInProcessing
                                                    .substring(curPos,
                                                            nextRsPos - 1));
                                    nctextrecord.setRecordId(recordId++);
                                    nctextrecord.setIssueSite(stnId); // replace
                                                                      // cccc
                                                                      // with
                                                                      // stndId
                                                                      // found
                                                                      // for
                                                                      // this
                                                                      // record
                                    reports.add(nctextrecord);

                                    // sMRawMsg =
                                    // sMRawMsg.substring(nextRsPos+1);//move
                                    // string to after current next RS
                                    // nextRsPos =
                                    // sMRawMsg.indexOf(ASCII_RS);//new next RS
                                    stnId = sRawMsgInProcessing.substring(
                                            nextRsPos + 1, nextRsPos + 5);
                                    // cumRsPos = cumRsPos + nextRsPos;
                                    curPos = nextRsPos + 1;

                                    nextRsPos = sRawMsgInProcessing.indexOf(
                                            ASCII_RS, nextRsPos + 1);// new next
                                                                     // RS
                                    // stnId = sMRawMsg.substring(0, 4);

                                }
                                // Handle the last record which is NOT saved in
                                // the while loop
                                NctextRecord nctextrecord = new NctextRecord();
                                setNctextRecord(nctextrecord);
                                // nctextrecord.setRawRecord(sMRawMsg);
                                nctextrecord.setRawRecord(sRawMsgInProcessing
                                        .substring(curPos, endPos - 1));
                                nctextrecord.setRecordId(recordId++);
                                nctextrecord.setIssueSite(stnId); // replace
                                                                  // cccc with
                                                                  // stndId
                                                                  // found for
                                                                  // this record
                                reports.add(nctextrecord);
                                // System.out.println(" number RS = "+
                                // testCount);
                            } else {
                                // Chin debug
                                System.out
                                        .println("setData for M type: find wmoHeader "
                                                + (recordId - 1)
                                                + ": "
                                                + WMOId
                                                + " "
                                                + cccc
                                                + " "
                                                + YYGGgg
                                                + " "
                                                + awipsId
                                                + " BUT, no RS ^^ found!");

                                rawMsglength = 0; ///* get out of here 
                            }
                            // System.out.println("M type report size : "+
                            // reports.size());

                        } else {
                            // other data type - I.e. B,Z,R,O,F, W type
                            int recordEndPos = endPos;
                            recordEndPos--; // exclude Ctl-C
                            // Adjust record end position by skipping CR and LN
                            // at end of record
                            while (recordEndPos > 0) {
                                if ((sRawMsgInProcessing.charAt(recordEndPos) == IDecoderConstants.ASCII_LF)
                                        || (sRawMsgInProcessing
                                                .charAt(recordEndPos) == IDecoderConstants.ASCII_CR)) {
                                    recordEndPos--;

                                } else {
                                    break;
                                }
                            }
                            NctextRecord nctextrecord = new NctextRecord();
                            setNctextRecord(nctextrecord);
                            nctextrecord.setReportData(sRawMsgInProcessing
                                    .substring(messageDataStart,
                                            recordEndPos + 1));
                            nctextrecord.setRawRecord(sRawMsgInProcessing
                                    .substring(recordStart, recordEndPos + 1));
                            nctextrecord.setRecordId(recordId++);

                            reports.add(nctextrecord);
                        }
                        // adjust pointers for next possible section/record
                        rawMsglength = rawMsglength - (endPos + 1); // string
                                                                    // index
                                                                    // start
                                                                    // with 0
                        rawMsgPointer = rawMsgPointer + (endPos + 1);
                        // Chin debug
                        // logger.info("setData : find wmoHeader "
                        // +(recordId-1)+ ": "
                        // + WMOId + " " + cccc + " " + YYGGgg+ " " + awipsId +
                        // " " + proType
                        // /*+"\n rawMsgLn " + rawMsglength + "rawMsgPointer " +
                        // rawMsgPointer + "endPos " + endPos);
                        resetVaraable();

                    } else {
                        // Chin debug
                        logger.info("setData : find wmoHeader "
                                + (recordId - 1) + ": " + WMOId + " " + cccc
                                + " " + YYGGgg + " " + awipsId
                                + " BUT, no message data end Ctl-C found! "//*
                                                                          //  * +
                                                                          //  * " rawMsgLn "
                                                                          //  * +
                                                                          //  * rawMsglength
                                                                          ///  * +
                                                                          //  * " rawMsgPointer "
                                                                          //  * +
                                                                          //  * rawMsgPointer
                                                                          //  * +
                                                                         //   * " endPos "
                                                                          //  * +
                                                                          //  * endPos
                                                                          //  );

                        rawMsglength = 0; ///* get out of here 
                    }
                }
                // Chin debug
                else {
                    int ePos;
                    switch (parseErr) {
                    case NO_AWIPS_HDR:
                        logger.info("setData : no header found. Stop here! ");
                        rawMsglength = 0;// get out of here
                        logErrToFile("No AWIPS header found in this file. Stop Parsing!\n");
                        break;
                    case NO_AWIPS_HDR_IN_BULLETIN:
                        logger.info("setData : no header found in a bulletin. ");
                        // Skip this part of data. adjust buffer to next
                        // possible bulletin
                        ePos = sRawMsgInProcessing
                                .indexOf(IDecoderConstants.ASCII_ETX); // Ctl-C
                        rawMsglength = rawMsglength - (ePos + 1); // string
                                                                  // index start
                                                                  // with 0
                        rawMsgPointer = rawMsgPointer + (ePos + 1);
                        logErrToFile("A Bulletin without good AWIPS header found. Skip it!\n");
                        break;
                    case NO_CTLA_IN_BULLETIN:
                        logger.info("setData : no ctl-A found in a bulletin. ");
                        // Skip this part of data. adjust buffer to next
                        // possible bulletin
                        ePos = sRawMsgInProcessing
                                .indexOf(IDecoderConstants.ASCII_ETX); // Ctl-C
                        rawMsglength = rawMsglength - (ePos + 1); // string
                                                                  // index start
                                                                  // with 0
                        rawMsgPointer = rawMsgPointer + (ePos + 1);
                        logErrToFile("A Bulletin without ctl-A found. Skip it!\n");
                        break;
                    case NO_CTLA_TO_END:
                        logger.info("setData : no ctl-A found from this point down. Stop here! ");
                        // Skip this part of data. adjust buffer to next
                        // possible bulletin
                        rawMsglength = 0;// get out of here
                        logErrToFile("No ctl-A found from this point down. Stop here!\n");
                        break;
                    case NO_CTLC_TO_END:
                        logger.info("setData : no ctl-C found from this point down. Stop here! ");
                        // Skip this part of data. adjust buffer to next
                        // possible bulletin
                        rawMsglength = 0;// get out of here
                        logErrToFile("No ctl-C found from this point down. Stop here!\n");
                        break;
                    case NO_ERR:
                        logger.info("setData : end of file. Stop here! ");
                        rawMsglength = 0;// get out of here
                        break;
                    default:
                        logger.info("setData : general error! Stop here! ");
                        logErrToFile("Unknow parsing error. Stop here!\n");
                        rawMsglength = 0;// get out of here
                        break;
                    }
                }

            }// While loop

        }

        if ((reports != null) && (reports.size() > 0)) {
            currentReport = 0;
        } else {
            // Chin debug
            logger.info(traceId + " - setData():No reports found in data.");
        }
        */
    }

    private synchronized boolean parseRcdHeader(String sInputMessageData) {
        // Assume not valid until proven otherwise!
        boolean isValid = false;
        // adjustBuffer = false;
        parseErr = ParseError.GENERAL_ERR;
        BBBIndicator = "REG";
        awipsId = "NA";

        if (sInputMessageData != null) {
            // CtlA is the start of a record bulletin and Ctl-C is the end of it
            int CtlAPos = sInputMessageData
                    .indexOf(IDecoderConstants.ASCII_SOM);
            // int CtlCPos =
            // sInputMessageData.indexOf(IDecoderConstants.ASCII_ETX);

            if ((CtlAPos == -1)
                    && (sInputMessageData.length() <= MIN_AWIPS_HDR_LENGTH)) {
                parseErr = ParseError.NO_ERR; // end of file
                return isValid;
            }
            if (CtlAPos == -1) {
                parseErr = ParseError.NO_CTLA_TO_END;
                return isValid;
            }
            String sMessageData = sInputMessageData.substring(CtlAPos);

            Pattern p = Pattern.compile(WMO_HEADER);
            Matcher m = p.matcher(sMessageData);
            if (m.find()) {

                messageDataStart = m.end();
                WMOId = m.group(WMOIDGROUP_NUMBER);
                cccc = m.group(CCCCGROUP_NUMBER);

                YYGGgg = m.group(YYGGggGROUP_NUMBER);
                if ((m.end() - m.end(YYGGggGROUP_NUMBER)) >= BBB_SIZE) {
                    String sBBB = sMessageData.substring(
                            m.end(YYGGggGROUP_NUMBER), m.end());
                    Pattern pBBB = Pattern.compile(BBBID);
                    Matcher mBBB = pBBB.matcher(sBBB);

                    // If there is an optional BBB;
                    if (mBBB.find()) {
                        BBBIndicator = mBBB.group();
                    }
                }
                // parse 2nd line to get AWIPSId nnnxxx
                String sl = sMessageData.substring(m.end());/*
                                                             * String from 2nd
                                                             * line down to end
                                                             */
                int endLineCR = sl.indexOf(IDecoderConstants.ASCII_CR);
                int endLineLF = sl.indexOf(IDecoderConstants.ASCII_LF);
                sl = sl.substring(0, Math.min(endLineCR, endLineLF));// String
                                                                     // of 2nd
                                                                     // line
                                                                     // only
                Pattern pAWIPS = Pattern.compile(AWIPS_ID);
                Matcher ml = pAWIPS.matcher(sl);
                if (ml.find()) {
                    awipsId = ml.group();
                    messageDataStart = ml.end();
                }
                // set Record start pointer (recordStart), remove CR, LN and Soh
                // (Ctl-A) from the record head
                recordStart = 0;
                int msgleng = sMessageData.length();
                while (msgleng > 0) {
                    if ((sMessageData.charAt(recordStart) == IDecoderConstants.ASCII_SOM)
                            || (sMessageData.charAt(recordStart) == IDecoderConstants.ASCII_LF)
                            || (sMessageData.charAt(recordStart) == IDecoderConstants.ASCII_CR)) {
                        // skip ASCII_SOM ( I think it is a typo in imported
                        // package, should be SOH, Ctl-A )
                        // also skip CR and LN
                        msgleng--;
                        recordStart++;
                    } else {
                        break;
                    }

                }
                isValid = true;

            } else {
                parseErr = ParseError.NO_AWIPS_HDR;
            }
        }
        return isValid;
    }
    private String[] getGdStnGp(String stnId){
    	for(String[]  stnGp: gdStnGp){
    		for(int i=0; i < stnGp.length; i++){
    			if(stnId.equals(stnGp[i])){
    				return stnGp;
    			}
    		}
    	}
    	return null;
    }

    /**
     * Set the raw message data and invoke the internal message separation
     * process.
     * 
     * @param rawMessage
     *            The raw weather text message.
     */

    public void setRecordData(byte[] rawMessage) {
        currentReport = -1;

        // Now check for some binary data types, Stop decoding, if it is binary
        String sRawMessage = new String(rawMessage);
        //some reports contains null char which will cause DB persistence error. Replace all null with
        // Space.
        sRawMessage = sRawMessage.replace((char)0x0, (char)0x20);
        int pos = sRawMessage.indexOf("BUFR");
        boolean notStored = false;
        notStored = notStored || ((pos >= 0) && (pos < 20));
        pos = sRawMessage.indexOf("GRIB");
        notStored = notStored || ((pos >= 0) && (pos < 20));
        pos = sRawMessage.indexOf("JPG");
        notStored = notStored || ((pos >= 0) && (pos < 20));
        pos = sRawMessage.indexOf("PNG");
        notStored = notStored || ((pos >= 0) && (pos < 20));
        pos = sRawMessage.indexOf("GIF87");
        notStored = notStored || ((pos >= 0) && (pos < 20));

        int recordId = 1; /* to record number of records with same AWIPS header */
        reports = new ArrayList<NctextRecord>();
        // String sRawMsgInProcessing = null;
        int endPos;
        int rsPos, nextRsPos, curPos;
        if (!notStored) {
            String fileType = nctextInputFileTypeDao
                    .getFiletypeType(ingestFileExt);
            String ctlC = new String();
            char[] data = { IDecoderConstants.ASCII_ETX };
            ctlC = String.copyValueOf(data);
            String[] strRcdArray = sRawMessage.split(ctlC);
            logger.info("number of rcds " + strRcdArray.length);
            for (String strRcd : strRcdArray) {

                if (parseRcdHeader(strRcd)) {
                    endPos = strRcd.length();
                    if (fileType.equals("M")) {
                        // M type data has several records in one "section".
                        // They are separated by Record Separator "^^".
                        rsPos = strRcd.indexOf(ASCII_RS); // find first RS
                        if ((rsPos >= 0) && (rsPos < endPos)) {
                        	int stnidEnd;
                        	
                        	stnidEnd = strRcd.substring(rsPos + 1).indexOf(ASCII_SP) + 1; //Chin fix mos stnid bug
                            
                            String stnId = strRcd.substring(rsPos + 1,
                                    rsPos + stnidEnd);
                            curPos = rsPos + 1;
                            nextRsPos = strRcd.indexOf(ASCII_RS, curPos); // 2nd
                                                                          // RS
                            while ((nextRsPos >= 0) && (nextRsPos < endPos)) {
                                NctextRecord nctextrecord = new NctextRecord();
                                setNctextRecord(nctextrecord);
                                nctextrecord.setRawRecord(strRcd.substring(
                                        curPos, nextRsPos - 1));
                                nctextrecord.setRecordId(recordId++);
                                nctextrecord.setIssueSite(stnId); // replace
                                                                  // cccc with
                                                                  // stndId
                                                                  // found for
                                                                  // this record
                                if(stnId.length() <= 8) //Chin : to make sure we do not get unwanted/bad record with longer than 8 chars stnid
                                	reports.add(nctextrecord);
                                stnidEnd = strRcd.substring(nextRsPos + 1).indexOf(ASCII_SP) + 1; //Chin fix mos stnid bug
                                stnId = strRcd.substring(nextRsPos + 1,
                                        nextRsPos + stnidEnd);
                                curPos = nextRsPos + 1;

                                nextRsPos = strRcd.indexOf(ASCII_RS,
                                        nextRsPos + 1);// new next RS
                            }
                            // Handle the last record which is NOT saved in the
                            // while loop
                            NctextRecord nctextrecord = new NctextRecord();
                            setNctextRecord(nctextrecord);
                            nctextrecord.setRawRecord(strRcd.substring(curPos,
                                    endPos - 1));
                            nctextrecord.setRecordId(recordId++);
                            nctextrecord.setIssueSite(stnId); // replace cccc
                                                              // with stndId
                                                              // found for this
                                                              // record
                            if(stnId.length() <= 8) 
                            	reports.add(nctextrecord);
                        } else {
                            // Chin debug
                            System.out
                                    .println("setRecordData for M type: find wmoHeader "
                                            + (recordId - 1)
                                            + ": "
                                            + WMOId
                                            + " "
                                            + cccc
                                            + " "
                                            + YYGGgg
                                            + " "
                                            + awipsId + " BUT, no RS ^^ found!");
                        }
                    }
                    else if (fileType.equals("R")){ 
                    	Pattern p = Pattern.compile(R_TYPE_SEPARATOR); 
                    	Matcher m =p.matcher(strRcd); 
                    	String stnIdFound="NA",  subStr; 
                    	String[] stnGp=null;
                    	boolean saveit = false; 
                    	if (m.find()) {
                    		stnIdFound = m.group(); 
                    		stnIdFound = stnIdFound.substring(0, stnIdFound.length()-2); 
                    		stnGp = getGdStnGp(stnIdFound); 
                    		if(stnGp !=null)
                    			saveit =true; 
                    	} else { //This record may have different format, Its Stn ID is one line before "00///" 
                    		p = Pattern.compile(R_TYPE_SEPARATOR2); 
                    		m = p.matcher(strRcd);
                    		if (m.find()) { 
                    			subStr = strRcd.substring(m.start()); 
                    			// find the first "00///" and move str to here 
                    			//from subStr, find a first stn id of this gp stns 
                    			//this stn is actually not the first stn in the record. but it is ok, we only need one stn id in the gp.
                    			p = Pattern.compile(R_TYPE_SEPARATOR3);
                    			m = p.matcher(subStr); 
                    			if (m.find()){ 
                    				stnIdFound = m.group(); 
                    				stnGp = getGdStnGp(stnIdFound); 
                    				if(stnGp !=null)
                    					saveit = true; 
                    			} else {
                    				logger.info("Could not find stn id in RGD file record !!!"); 
                    			} 
                    		} else {
                    			logger.info("Could not find stn id in RGD file record!" ); 
                    		} 
                    	} 
                    	if(saveit == true && stnGp != null) {
                    		//For consistent with all other text record, we have to save smae record for each stn in the gp,
                    		// so appication can query record based on stnId
                    		for(String stn: stnGp){
                    			NctextRecord nctextrecord = new NctextRecord();
                    			setNctextRecord(nctextrecord);
                    			nctextrecord.setRawRecord(strRcd.substring(recordStart,endPos-1)); 
                    			nctextrecord.setRecordId(recordId++);
                    			nctextrecord.setIssueSite(stn); 
                    			// replace cccc with group stndId found for this record
                    			reports.add(nctextrecord); 
                    		}
                    	} else 
                    		logger.info("stn id "+ stnIdFound + " But gp Stn is not found");
                    }                 
                    else if (fileType.equals("O")) {
                        logger.info("Observer data is not supported now!!!");
                    } else {
                        // other data types - I.e. B, W, Z, F types
                        NctextRecord nctextrecord = new NctextRecord();
                        setNctextRecord(nctextrecord);
                        nctextrecord.setRawRecord(strRcd.substring(recordStart,
                                endPos - 1));
                        nctextrecord.setRecordId(recordId++);
                        if (fileType.equals("F")) {
                            // use AWIPS id as issue set for this type of file,
                            // e.g. FFG file
                            nctextrecord.setIssueSite(awipsId);
                        }
                        reports.add(nctextrecord);
                    }
                    resetVaraable();

                } else {
                    // Chin debug
                    logger.info("setRecordData : find wmoHeader "
                            + (recordId - 1) + ": " + WMOId + " " + cccc + " "
                            + YYGGgg + " " + awipsId
                            + " BUT, some issues found! ");
                    switch (parseErr) {
                    case NO_AWIPS_HDR:
                        logger.info("setRecordData : no header found. Stop here! ");
                        logErrToFile("No AWIPS header found in this file. Stop Parsing!\n");
                        break;
                    case NO_AWIPS_HDR_IN_BULLETIN:
                        logger.info("setRecordData : no header found in a bulletin. ");
                        // Skip this part of data. adjust buffer to next
                        // possible bulletin
                        logErrToFile("A Bulletin without good AWIPS header found. Skip it!\n");
                        break;
                    case NO_CTLA_IN_BULLETIN:
                        logger.info("setRecordData : no ctl-A found in a bulletin. ");
                        // Skip this part of data. adjust buffer to next
                        // possible bulletin
                        logErrToFile("A Bulletin without ctl-A found. Skip it!\n");
                        break;
                    case NO_CTLA_TO_END:
                        logger.info("setRecordData : no ctl-A found from this point down. Stop here! ");
                        // Skip this part of data. adjust buffer to next
                        // possible bulletin
                        logErrToFile("No ctl-A found from this point down. Stop here!\n");
                        break;
                    case NO_CTLC_TO_END:
                        logger.info("setRecordData : no ctl-C found from this point down. Stop here! ");
                        // Skip this part of data. adjust buffer to next
                        // possible bulletin
                        logErrToFile("No ctl-C found from this point down. Stop here!\n");
                        break;
                    case NO_ERR:
                        logger.info("setRecordData : end of file. Stop here! ");
                        break;
                    default:
                        logger.info("setRecordData : general error! Stop here! ");
                        logErrToFile("Unknow parsing error. Stop here!\n");
                        break;
                    }
                }

            }

        }

        if ((reports != null) && (reports.size() > 0)) {
            currentReport = 0;
        } else {
            // Chin debug
            logger.info(traceId + " - setRecordData():No reports found in data.");
        }
    }

    private void resetVaraable() {
        BBBIndicator = null;
        WMOId = null;
        awipsId = null;
        cccc = null;
        YYGGgg = null;

    }

    private void setNctextRecord(NctextRecord nctextrecord) {
        nctextrecord.setAwipsId(awipsId);
        nctextrecord.setBbbInd(BBBIndicator);
        nctextrecord.setWmoId(WMOId);
        nctextrecord.setIssueSite(cccc);
        Calendar cal = null;
        try {
            cal = Util.findCurrentTime(YYGGgg);

        } catch (DataFormatException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        if (traceId.length() >= YYYYMMDD_LENGTH) {
            try {
                // Adjust year and month from ingest file name
                String date = traceId.substring(0, YYYYMMDD_LENGTH);
                int iDay = Integer.parseInt(date.substring(6, 8).trim());
                int iMonth = Integer.parseInt(date.substring(4, 6).trim());
                int iYear = Integer.parseInt(date.substring(0, 4).trim());
                // Note: month's index starts from 0
                cal.set(iYear, iMonth - 1, iDay);
            } catch (NumberFormatException n) {
                // do nothing, if can't get yy/mon/day info from file name, just
                // use current date
            }

        }
        nctextrecord.setIssueTime(cal);
        nctextrecord.setDataTime(new DataTime(cal));

        // set data productType,
        // String proType = nctextProductDao.getProductType(WMOId);
        nctextrecord.setProductType(ingestFileExt);

    }

    /**
     * @return the traceId
     */
    public String getTraceId() {
        return traceId;
    }

    /**
     * @param traceId
     *            the traceId to set
     */
    public void setTraceId(String traceId) {
        this.traceId = traceId;
    }

}
