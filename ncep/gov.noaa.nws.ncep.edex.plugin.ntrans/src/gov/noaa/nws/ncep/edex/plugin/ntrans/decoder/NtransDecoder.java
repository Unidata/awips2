package gov.noaa.nws.ncep.edex.plugin.ntrans.decoder;

import gov.noaa.nws.ncep.common.dataplugin.ntrans.NtransRecord;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.time.DataTime;

/**
 * NTRANS Decoder
 * 
 * This class decodes legacy NTRANS Metafiles.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#  Author      Description
 * ------------ -------- ----------- -------------------------------------
 * 03/2013               B. Hebbard  Initial creation
 * 04/2013               B. Hebbard  IOC version (for OB13.4.1)
 * 10/2013               B. Hebbard  Modify model name inference from metafile name
 * Aug 30, 2013 2298     rjpeter     Make getPluginName abstract
 * 6/2014                T. Lee      Added HYSPLIT and fixed "other" modelName
 * 08/2014               B. Hebbard  Revise createDataTime() to correct end-of-month boundary bug
 * 08/2014               B. Hebbard  Enhance to use (cycle) time info from metafile name, if available
 * 09/2014               B. Hebbard  Normalize (shorten) metafile name to remove directory artifacts added during dataflow, so user will see that they're used to and will fit selection dialog column
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
public class NtransDecoder extends AbstractDecoder {

    private final static int NTRANS_FILE_TITLE_SIZE = 32; // bytes

    private final static int NTRANS_FRAME_LABEL_SIZE = 64;

    private final static int NTRANS_FRAME_LABEL_TIME_SUBSTRING_SIZE = 9;

    private final static int NTRANS_RESERVED_SPACE_SIZE = 38;

    Calendar decodeTime = null;

    private String normalizedMetafileName;

    Integer yearFromFileName = null;

    Integer monthFromFileName = null;

    Integer dateFromFileName = null;

    Integer hourFromFileName = null;

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public NtransDecoder() throws DecoderException {
        decodeTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
    }

    private class FrameHeader {
        String frameHeaderTimeString;

        String productNameString;

        long startPos;

        long endPos;
    }

    /**
     * 
     * @param inputFile
     *            : ingest file to be decoded
     * @return
     * @throws DecoderException
     * @throws PluginException
     * 
     */
    public// synchronized
    PluginDataObject[] decode(File inputFile) throws DecoderException,
            PluginException {
        byte[] fileData = null;
        byte[] headerData = null;
        InputStream inputStream = null;
        String fileTitle = null;
        int fileMaxFrame = 0;
        int fileVersion = 0;
        int fileMachineType = 0;
        int frameSizeX = 0;
        int frameSizeY = 0;
        byte[] fileReservedSpace = new byte[NTRANS_RESERVED_SPACE_SIZE]; // TODO
                                                                         // symbolic

        List<FrameHeader> frameHeaders = new ArrayList<FrameHeader>();
        List<NtransRecord> records = new ArrayList<NtransRecord>();

        try {

            decodeTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

            fileName = inputFile.getName();

            normalizedMetafileName = normalizeMetafileName(fileName);

            getTimeFromMetafileName(normalizedMetafileName);

            inputStream = new FileInputStream(inputFile);

            // Read the entire file

            // TODO: Wish we didn't have to do that, but decode method
            // wants to return all PDOs at once, so it's all got
            // to sit in memory anyway. Propose architecture change (?)
            // say, decode(File,IDataReturners) to allow sending up
            // PDOs as completed, so memory can be released?
            // Anyway, this allows us to use NIO buffer tricks...

            long inputFileCount = inputFile.length();
            if (inputFileCount > Integer.MAX_VALUE) {
                throw new DecoderException("Input file too big! "
                        + inputFileCount + " bytes");
            }
            byte[] fileBytes = new byte[(int) inputFileCount];
            int fileBytesReadIn = inputStream.read(fileBytes);
            if (fileBytesReadIn != inputFileCount) {
                throw new DecoderException(
                        "Input file not read correctly/completely! "
                                + fileBytesReadIn + " of " + inputFileCount
                                + " bytes read");
            }

            ByteBuffer byteBuffer = ByteBuffer.wrap(fileBytes);

            byteBuffer.order(determineEndianess(byteBuffer));

            byteBuffer.rewind();

            // Read NTRANS metafile header

            byte[] fileTitleBytes = new byte[NTRANS_FILE_TITLE_SIZE];
            byteBuffer.get(fileTitleBytes);
            fileTitle = new String(fileTitleBytes).trim();
            // System.out.println("[File title:  " + fileTitle + "]");

            fileMaxFrame = toUnsigned(byteBuffer.getShort());
            fileVersion = toUnsigned(byteBuffer.getShort());
            fileMachineType = toUnsigned(byteBuffer.getShort());
            frameSizeX = toUnsigned(byteBuffer.getShort());
            frameSizeY = toUnsigned(byteBuffer.getShort());

            byte[] fileReserved = new byte[NTRANS_RESERVED_SPACE_SIZE];
            byteBuffer.get(fileReserved);

            // Read NTRANS frame headers (follow file header; precede frame
            // contents)

            for (int frame = 0; frame < fileMaxFrame; frame++) {
                byte[] labelTitleBytes = new byte[NTRANS_FRAME_LABEL_SIZE];
                byteBuffer.get(labelTitleBytes);
                StringBuffer sb = new StringBuffer();
                for (int i = 0; (i < NTRANS_FRAME_LABEL_SIZE)
                        && (labelTitleBytes[i] != 0x00); i++) {
                    sb.append((char) labelTitleBytes[i]);
                }
                String labelTitle = new String(sb);
                long startPos = toUnsigned(byteBuffer.getInt());
                long endPos = toUnsigned(byteBuffer.getInt());
                if ((startPos == 0) && (endPos == 0)) {
                    break;
                }
                // System.out.println("[startPos " + startPos + " endPos " +
                // endPos + "]");
                FrameHeader fh = new FrameHeader();
                if (labelTitle.length() < NTRANS_FRAME_LABEL_TIME_SUBSTRING_SIZE) {
                    fh.frameHeaderTimeString = labelTitle;
                    fh.productNameString = "";
                } else {
                    fh.frameHeaderTimeString = labelTitle.substring(0,
                            NTRANS_FRAME_LABEL_TIME_SUBSTRING_SIZE);
                    fh.productNameString = labelTitle
                            .substring(NTRANS_FRAME_LABEL_TIME_SUBSTRING_SIZE);
                }
                fh.startPos = startPos;
                fh.endPos = endPos;
                frameHeaders.add(fh);
            }

            // Read NTRANS frames (CGM coded images)

            // for (FrameHeader fh : frameHeaders) {
            for (int frame = 0; frame < frameHeaders.size(); frame++) {
                FrameHeader fh = frameHeaders.get(frame);
                int startPos = (int) (fh.startPos - 4);
                int endPos = (int) (fh.endPos + 0);
                int imageLength = (endPos - startPos) + 0;
                byte[] frameImage = new byte[imageLength];
                // int imageBytesRead = inputStream.read(frameImage);
                byteBuffer.position(startPos);
                byteBuffer.get(frameImage);

                // Create NTRANS record (PDO)

                NtransRecord record = new NtransRecord();

                record.setReportType("NTRANS");
                record.setModelName(inferModel(inputFile.getName()).replaceAll(
                        "_", "-"));
                record.setMetafileName(normalizedMetafileName.replaceAll("_",
                        "-"));
                record.setProductName(fh.productNameString
                        .trim()
                        .replaceAll("_", "-")
                        // TODO: Cleanup/Combine in regex. (Wanted to test one
                        // at a time.)
                        .replaceAll(" ", "-").replaceAll(":", "-")
                        .replaceAll("\\.", "-").replaceAll("&", "and")
                        // TODO acceptable??
                        .replaceAll(",", "-").replaceAll("--", "-")
                        .replaceAll("--", "-")); // twice
                record.setDataTime(createDataTime(fh.frameHeaderTimeString));
                record.setValidTimeString(fh.frameHeaderTimeString);
                record.setImageData(frameImage);
                record.setImageSizeX(frameSizeX);
                record.setImageSizeY(frameSizeY);
                record.setImageByteCount(frameImage.length);
                record.setFrameNumberInFile(frame);
                record.setTotalFramesInFile(frameHeaders.size());

                if (record != null) {
                    try {
                        record.constructDataURI();
                    } catch (PluginException e) {
                        throw new DecoderException(
                                "Error constructing dataURI", e);
                    }
                }

                records.add(record);
            }

        } catch (IOException ioe) {
            logger.error("Error reading input file " + inputFile.getName(), ioe);
            fileData = null;
        } catch (Exception e) {
            fileData = null;
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException ioe) {
                    logger.error("Could not close input file "
                            + inputFile.getName());
                }
            }
        }

        if (records.isEmpty()) {
            return new PluginDataObject[0];
        } else {
            PluginDataObject[] pdos = records.toArray(new PluginDataObject[0]);
            return pdos;
        }

    }

    public String normalizeMetafileName(String fileName) {
        // Given..."gfs_gfs.20140901_gfs_20140901_12_ak"
        // Want....................."gfs_20140901_12_ak"
        //
        // @formatter:off
        // Darn... Following is thwarted by these cases:
        //   gfs_gfs.20140901_gfsver_20140901_18_na_mar
        //   ukmet.2014090_ukmet.2014090._ukmetver_20140901_00
        //   wave_wave.20140901_nww3_20140901_12
        //   wave_wave.20140902_nww3_20140902_00_akw
        //
        // final Pattern p = Pattern.compile("^(\\w+)_\\1\\.(\\d{6,8})_\\1_\\2");
        // Matcher m = p.matcher(fileName);
        // if (m.find()) {
        //     fileName = fileName.replaceFirst("^(\\w+)_\\1\\.(\\d{6,8})_", "");
        // }
        //
        // @formatter:on
        // So, instead we...

        // If the string constains a ".", then remove everything from start of
        // string through the FIRST "_" following the LAST "."

        if (fileName.contains(".")) {
            String[] splits = fileName.split("\\.");
            String lastSplit = splits[splits.length - 1];
            // "reluctant" (?) match to assure first "_"
            return lastSplit.replaceFirst("^.*?_", "");
        } else {
            return fileName;
        }
    }

    private int normalizeYear(int shortYear) {

        // Year can be 2 digits. If so, select century to make it the one
        // closest to the current year.

        // (Yeah, I know it's overkill. But that's what we thought *last*
        // century...)

        if (shortYear > 99) {
            return shortYear;
        } else {
            if (decodeTime == null) {
                decodeTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            }
            int currYear = decodeTime.get(Calendar.YEAR);
            int currCentury = currYear / 100;
            int breakpoint = (currYear + 50) % 100;
            int derivedYear = currCentury * 100 + shortYear;
            if (shortYear > breakpoint) {
                derivedYear++;
            }
            return derivedYear;
        }
    }

    private int CalendarMonth(int goodOldMonthNumber) {
        // Just to be proper (since we don't control the values
        // of these "magic constants", and so can't assume they
        // won't change out from under us (say, in the unlikely
        // but possible case that Calendar decides to go with
        // 1-based months (like the rest of the world) instead
        // of 0-based ones. (We're 0-based [array] here, too, but
        // WE control the order, and shield it from the caller.)
        // @formatter:off
        final int[] CalendarMonthConstants = {
            Calendar.JANUARY,
            Calendar.FEBRUARY,
            Calendar.MARCH,
            Calendar.APRIL,
            Calendar.MAY,
            Calendar.JUNE,
            Calendar.JULY,
            Calendar.AUGUST,
            Calendar.SEPTEMBER,
            Calendar.OCTOBER,
            Calendar.NOVEMBER,
            Calendar.DECEMBER,
        };
        return CalendarMonthConstants[goodOldMonthNumber - 1];
        // @formatter:on
    }

    private void getTimeFromMetafileName(String fileName) {

        // NTRANS metafile names (almost?) always contain date and (sometimes)
        // hour information. We assume that this refers to the cycle time of the
        // model run which produced the images it contains. Since individual
        // frame headers (contained within the body of the file) currently
        // (2014-08) provide only valid times with date -- and no month or year
        // -- taking these 'hints' from the file name, if available, allows us
        // to handle legacy data properly, even if months or years old.
        //
        // This method operates on instance variables for both input and output.
        //
        // Input is fileName, which is assumed to have been set to the full name
        // of the metafile currently being ingested, containing a substring of
        // one of the following forms:
        // @formatter:off
        //     YYYYMMDD
        //     YYYYMMDDHH
        //     YYYYMMDD_HH
        //     YYMMDD
        //     YYMMDD_HH
        // @formatter:on
        //
        // Output takes the form of...
        // yearFromFileName, monthFromFileName,
        // dateFromFileName, hourFromFileName
        // These are (boxed) Integer variables; a null value indicates no value
        // is available for that field.

        final Pattern p = Pattern.compile("((\\d\\d){3,4})_?(\\d\\d)?");
        Matcher m = p.matcher(fileName);

        String matchString = "";
        String hourString = "";

        String year = "";
        String month = "";
        String date = "";
        String hour = "";

        while (m.find()) {
            if (m.group(0).length() >= matchString.length()) {
                matchString = m.group(0);

                String dateString = m.group(1).replaceFirst("_", "");

                date = dateString.substring(dateString.length() - 2);
                dateString = dateString.substring(0, dateString.length() - 2);
                month = dateString.substring(dateString.length() - 2);
                dateString = dateString.substring(0, dateString.length() - 2);
                year = dateString.substring(dateString.length() - 2);
                dateString = dateString.substring(0, dateString.length() - 2);
                assert (dateString.isEmpty());

                hourString = m.group(3);
            }
        }

        if (!matchString.isEmpty()) {
            try {
                yearFromFileName = normalizeYear(Integer.parseInt(year));
                monthFromFileName = Integer.parseInt(month);
                dateFromFileName = Integer.parseInt(date);
                if (!hourString.isEmpty()) {
                    hourFromFileName = Integer.parseInt(hourString);
                }
            } catch (Exception e) {
                // TODO: ERROR
                // Set *FromFileName back to null? Or not (leave partial partial
                // parse results)?
            }
        }
    }

    DataTime createDataTime(String frameHeaderTimeString,
            String simulatedFileName, Calendar simulatedDecodeTime) {
        decodeTime = simulatedDecodeTime;
        return createDataTime(frameHeaderTimeString, simulatedFileName);
    }

    DataTime createDataTime(String frameHeaderTimeString,
            String simulatedFileName) {
        fileName = simulatedFileName;
        normalizedMetafileName = normalizeMetafileName(fileName);
        getTimeFromMetafileName(normalizedMetafileName);
        return createDataTime(frameHeaderTimeString);
    }

    DataTime createDataTime(String frameHeaderTimeString) {

        // Create a standard DataTime object, with proper timing
        // determined from given time string (e.g., "27/06V042")
        // (from the frame header), AND fields parsed earlier from
        // the metafile name (and stored in instance variables),
        // if available.

        // --

        // Get components of validframeHeaderTimeString as 'int's

        // Be able to decode...
        // 8, 6, 4, or 2 digits, followed by...
        // "/" followed by...
        // 2 digits followed by...
        // "F" or "V" followed by...
        // any number of digits? 2-3? 1-6?
        // NOT with intervening spaces?

        final Pattern p = Pattern
                .compile("((\\d\\d){1,4})/(\\d\\d)(F|V)(\\d{1,4})");
        Matcher m = p.matcher(frameHeaderTimeString);

        boolean isV = false;

        Integer centuryFromFrameHeader = null;
        Integer yearFromFrameHeader = null;
        Integer monthFromFrameHeader = null;
        Integer dateFromFrameHeader = null;
        Integer hourFromFrameHeader = null;
        Integer fcstHour = null;

        String dateString, hourString, fOrV, fcstHourString;

        if (m.find()) {
            dateString = m.group(1);
            hourString = m.group(3);
            fOrV = m.group(4);
            fcstHourString = m.group(5);
        } else {
            return new DataTime(decodeTime, 0); // should be error indication
        }

        try {
            switch (dateString.length()) {
            case 8:
                centuryFromFrameHeader = Integer.parseInt(dateString.substring(
                        0, 2));
                dateString = dateString.substring(2);
                // NO break;
            case 6:
                yearFromFrameHeader = Integer.parseInt(dateString.substring(0,
                        2));
                dateString = dateString.substring(2);
                if (centuryFromFrameHeader == null) {
                    yearFromFrameHeader = normalizeYear(yearFromFrameHeader);
                } else {
                    yearFromFrameHeader += centuryFromFrameHeader * 100;
                }
                // NO break;
            case 4:
                monthFromFrameHeader = Integer.parseInt(dateString.substring(0,
                        2));
                dateString = dateString.substring(2);
                // NO break;
            case 2:
                dateFromFrameHeader = Integer.parseInt(dateString.substring(0,
                        2));
                dateString = dateString.substring(2);
                assert (dateString.isEmpty());
                break;
            default:
                // ERROR
                break;
            }
            hourFromFrameHeader = Integer.parseInt(hourString);
            isV = fOrV.equalsIgnoreCase("V");
            fcstHour = Integer.parseInt(fcstHourString);
        } catch (Exception e) {
            // TODO
            return new DataTime(Calendar.getInstance(TimeZone
                    .getTimeZone("GMT")), 0);
        }

        // Establish upper bound on what the specified time means.
        // For F-type string, that would be the cycle time.
        // For V-type string, that would be the valid time.

        // Start with the decode (system) time...
        Calendar upperBoundTime = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        upperBoundTime.setTime(decodeTime.getTime());

        // YEAR: If specified in the frame header, that takes priority...
        if (yearFromFrameHeader != null) {
            upperBoundTime.set(Calendar.YEAR, yearFromFrameHeader);
        }
        // ...otherwise use value from file name, if available...
        else if (yearFromFileName != null) {
            upperBoundTime.set(Calendar.YEAR, yearFromFileName);
        }
        // ...otherwise defaults to the current year.

        // MONTH: If specified in the frame header, that takes priority...
        if (monthFromFrameHeader != null) {
            upperBoundTime.set(Calendar.MONTH,
                    CalendarMonth(monthFromFrameHeader));
        }
        // ...otherwise use value from file name, if available
        else if (monthFromFileName != null) {
            upperBoundTime
                    .set(Calendar.MONTH, CalendarMonth(monthFromFileName));
        }
        // ...otherwise defaults to current month

        // DATE: If specified in the frame header, IGNORE FOR NOW...
        // if (dateFromFrameHeader != null) {
        // upperBoundTime.set(Calendar.DAY,
        // CalendarMonth(monthFromFrameHeader));
        // }
        // else
        // ...BUT do use value from file name, if available
        if (dateFromFileName != null) {
            upperBoundTime.set(Calendar.DAY_OF_MONTH, dateFromFileName);
        }
        // ...otherwise defaults to current date

        // If we're dealing with a V-type string, we're determining the VALID
        // time. Add forecast hours.
        if (isV) {
            upperBoundTime.add(Calendar.HOUR_OF_DAY, fcstHour);
        }

        // Now calculate the actual valid time, starting with the latest
        // possible...

        Calendar calculatedValidTime = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        calculatedValidTime.setTime(upperBoundTime.getTime());

        // ...setting the date field to the specified date...

        calculatedValidTime.set(Calendar.DATE, dateFromFrameHeader);

        // ...but if greater than the latest possible valid date...

        int latestPossibleValidDate = upperBoundTime.get(Calendar.DAY_OF_MONTH);
        if (dateFromFrameHeader > latestPossibleValidDate) {
            // ...then it must be that date in the PRIOR month...
            calculatedValidTime.add(Calendar.MONTH, -1);
        }

        // Now set the hour field to the specified hour...

        calculatedValidTime.set(Calendar.HOUR_OF_DAY, hourFromFrameHeader);

        // ...and finally set sub-hour fields all to zero

        calculatedValidTime.set(Calendar.MINUTE, 0);
        calculatedValidTime.set(Calendar.SECOND, 0);
        calculatedValidTime.set(Calendar.MILLISECOND, 0);

        // Now calculate actual initial (reference) or cycle time

        Calendar calculatedCycleTime = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        calculatedCycleTime.setTime(calculatedValidTime.getTime());

        // Subtract forecast hours to get initial (reference) or cycle time
        if (isV) {
            calculatedCycleTime.add(Calendar.HOUR_OF_DAY, -fcstHour);
        }

        // Sanity check against file name date and hour (if known)

        int calculatedCycleDate = calculatedCycleTime
                .get(Calendar.DAY_OF_MONTH);
        int calculatedCycleHour = calculatedCycleTime.get(Calendar.HOUR_OF_DAY);
        if (dateFromFileName != null && dateFromFileName != calculatedCycleDate) {
            // WARNING!!
            logger.warn("Cycle date " + dateFromFileName
                    + " from metafile name " + fileName + " differs from "
                    + calculatedCycleDate + " inferred from frame header "
                    + frameHeaderTimeString);
        } else if (hourFromFileName != null
                && hourFromFileName != calculatedCycleHour) {
            // WARNING!!
            logger.warn("Cycle hour " + hourFromFileName
                    + " from metafile name " + fileName + " differs from "
                    + calculatedCycleHour + " inferred from frame header "
                    + frameHeaderTimeString);
        }

        // Return DataTime, constructed from cycle time and forecast hour.

        DataTime dataTime = new DataTime(calculatedCycleTime, fcstHour * 3600);

        return dataTime;
    }

    private enum Model {
        // TODO - Remove this, to make decoder agnostic w.r.t. list of available
        // models.

        // We do this temporarily because we don't yet know the possible formats
        // of filename strings we're going to be fed, so for now we just look
        // for known model names appearing anywhere in the file name.
        // NOTE: Sequence is important only insofar as any model name must
        // appear after all model names of which it is a proper substring.
        // Also, OPC_ENC comes first, since its metafiles may contain other
        // model substrings

        // @formatter:off
        OPC_ENS,
        CMCE_AVGSPR,
        CMCE,
        CMCVER,
        CMC,
        CPC,
        DGEX,
        ECENS_AVGSPR,
        ECENS,
        ECMWFVER,
        ECMWF_HR,
        ECMWF,
        ENSVER,
        FNMOCWAVE,
        GDAS,
        GEFS_AVGSPR,
        GEFS,
        GFSP,
        GFSVERP,
        GFSVER,
        GFS,
        GHM,
        HPCQPF,
        HPCVER,
        HWRF,
        ICEACCR,
        JMAP,
        JMA,
        MEDRT,
        NAEFS,
        NAM20,
        NAM44,
        NAMVER,
        NAM,
        NAVGEM,
        NOGAPS,
        NWW3P,
        NWW3,
        RAPP,
        RAP,
        SREFX,
        SST,
        UKMETVER,
        UKMET,
        VAFTAD
        // @formatter:on
    };

    private String inferModel(String fileName) {
        // Infer the model name from the file name
        // (Use heuristics gleaned from $NTRANS_META contents)
        // TODO -- continuous improvement!

        String modelName = "other"; // TODO "default" model...?
        if (fileName.startsWith("ecens_prob")) {
            modelName = "ecens";
        } else if (fileName.startsWith("Day") || fileName.startsWith("Night")
                || fileName.startsWith("Official")) {
            modelName = "medrt";
        } else if (fileName.startsWith("meta_sst")) {
            modelName = "sst";
        } else if (fileName.startsWith("hysplit")) {
            modelName = "HYSPLIT";
        } else if (fileName.contains("_GFS")) {
            modelName = "vaftad";
            /*
             * } else if (fileName.contains("_2")) { modelName =
             * fileName.substring(0, fileName.indexOf("_2")); if
             * (modelName.equals("jma")) { modelName = "jmap"; } }
             * 
             * return modelName;
             */

        } else {
            for (Model model : Model.values()) {
                if (fileName.toLowerCase().contains(model.name().toLowerCase())) {
                    modelName = model.name().toLowerCase();
                    break;
                }
            }
            if (modelName.equals("jma")) {
                modelName = "jmap";
            }
            return modelName;
        }
        return modelName;
    }

    private ByteOrder determineEndianess(ByteBuffer byteBuffer) {
        // TODO Review This -- I think this is the same criterion
        // used in legacy; see scan_hmeta in process_meta.c
        // (where do_flip is set)
        byte lowAddressByteOfTheMachineTypeField = byteBuffer.get(36);
        if (lowAddressByteOfTheMachineTypeField == 0) {
            return ByteOrder.BIG_ENDIAN;
        } else {
            return ByteOrder.LITTLE_ENDIAN;
        }
    }

    private int toUnsigned(short shorty) {
        return shorty & 0xffff;
    }

    private long toUnsigned(int inty) {
        return inty & 0xffffffffL;
    }

}
