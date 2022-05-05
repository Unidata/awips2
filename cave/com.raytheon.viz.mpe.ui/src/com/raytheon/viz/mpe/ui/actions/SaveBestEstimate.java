/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 7, 2015  16954      cgobs      Fix for cv_use issue - using getFieldName() in certain parts.
 * Feb 4, 2015  17094      cgobs      Fix for fieldType being too long for mapx_field_type column in RWResult table.
 * Nov 05, 2015 15045      snaples    Added resourceChanged call at end of save method to read in any edits.
 * </pre>
 **/
package com.raytheon.viz.mpe.ui.actions;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;
import javax.imageio.stream.FileImageOutputStream;

import com.raytheon.uf.common.dataplugin.shef.tables.Rwresult;
import com.raytheon.uf.common.mpe.constants.FilePermissionConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.common.xmrg.XmrgFile.XmrgHeader;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.viz.hydrocommon.whfslib.IHFSDbGenerated;
import com.raytheon.viz.mpe.MPEDateFormatter;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEProcessGrib;
import com.raytheon.viz.mpe.ui.Activator;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.polygon.PolygonEditManager;

/**
 * 
 * Contains logic for saving best estimate data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2012            mschenke     Initial creation
 * Feb 2, 2014  16201      snaples      Added saved data flag support
 * 
 * Apr29, 2014  16308      lbousaidi    transmit RFC Bias when an hour
 *                                      MPE is saved via the GUI.
 * Mar 10, 2015 14554      snaples      Added check to remove Best Estimate polygons after saving.
 * Oct 19, 2015 18090      lbousaidi    Added date format when the token st3_date_form token is set to Ymd.
 * Aug 07, 2017 6334       bkowal       Directories are now created with 770 permissions and files 660.
 * </pre>
 * 
 * @author mschenke
 */
public class SaveBestEstimate {

    private static final String PROCESS_FLAG_LOCAL = "MPM01   ";

    private static final String PROCESS_FLAG_SBN = "QPE01   ";

    /**
     * Saves bestEstField as the best estimate data for the editDate. The
     * fieldData array is the data for the field to use with any edits applied
     * and the screenShot is a screenshot of the display the displayedField is
     * on and will be saved out to images if preferences set
     * 
     * @param editDate
     * @param bestEstField
     * @param fieldData
     * @param screenShot
     */
    public static void saveBestEstimate(Date editDate,
            DisplayFieldData bestEstField, short[] fieldData,
            BufferedImage screenShot) {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();

        String ST3_FORMAT_STRING = MPEDateFormatter.yyyyMMddHH;
        String date_form = appsDefaults.getToken("st3_date_form");
        if ((date_form == null) || date_form.isEmpty()
                || date_form.equals("mdY")) {
            ST3_FORMAT_STRING = MPEDateFormatter.MMddyyyyHH;
        } else if (date_form.equals("Ymd")) {
            ST3_FORMAT_STRING = MPEDateFormatter.yyyyMMddHH;
        }
        /*----------------------------------------------------------*/
        /* create date in desired format for use in xmrg filename */
        /*----------------------------------------------------------*/
        String cdate = MPEDateFormatter.format(editDate, ST3_FORMAT_STRING);

        /*----------------------------------------------------------*/
        /* create full pathname of binary file (input to MAPX) */
        /* filename_xmrg is used by grib encoder */
        /*----------------------------------------------------------*/
        String fileName = String.format("%s/xmrg%sz",
                appsDefaults.getToken("rfcwide_xmrg_dir"), cdate);

        String fileNameXmrg = String.format("xmrg%sz", cdate);

        /*----------------------------------------------------------*/
        /* update RWResult table */
        /* write merged (mosaicked) field to file */
        /* replace missing values with 0.0 */
        /*----------------------------------------------------------*/

        String rfc = MPEDataManager.getInstance().getRFC();

        update_rwr_save(rfc, editDate, bestEstField.getFieldName());

        XmrgFile xmrgFile = MPEDisplayManager.getXmrgFile(bestEstField,
                editDate);
        try {
            xmrgFile.load();
        } catch (IOException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    "Could not load xmrg file to save", e);
            return;
        }
        XmrgHeader header = xmrgFile.getHeader();
        try {
            short[] data = Arrays.copyOf(fieldData, fieldData.length);
            // Make copy since we will are editing here
            for (int i = 0; i < data.length; i++) {
                if (data[i] < 0) {
                    data[i] = 0;
                }
            }
            xmrgFile.setData(data);
            header.setProcessFlag(PROCESS_FLAG_LOCAL);
            xmrgFile.save(new File(fileName),
                    FilePermissionConstants.POSIX_FILE_SET);

            /*
             * Is the mpe_send_qpe_to_sbn token set to ON? If so create a second
             * QPE file with the proc_flag set to QPE01.
             */

            if (appsDefaults.getBoolean("mpe_send_qpe_to_sbn", false)) {
                fileName = String.format("%s/xmrg%sz",
                        appsDefaults.getToken("mpe_qpe_sbn_dir"), cdate);
                header.setProcessFlag(PROCESS_FLAG_SBN);
                xmrgFile.save(new File(fileName),
                        FilePermissionConstants.POSIX_FILE_SET);
            }
        } catch (IOException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    "Error saving xmrg data for " + header.getProcessFlag(), e);
        }

        /*-----------------------------------------------------------*/
        /* generate and save files depending on values of */
        /* mpe_save_... tokens read from .Apps_defaults */
        /*                                                           */
        /* create filenames for netCDF, gif, grib and jpeg files */
        /* if mpe_xxxx_id token not found or blank, then no string */
        /* is prepended to filename */
        /* in all cases, filenames contain date in form yyyymmddhh */
        /*-----------------------------------------------------------*/

        cdate = MPEDateFormatter.format_yyyyMMddHH(editDate);

        /*--------------------------------------*/
        /* generate and save gif image */
        /*--------------------------------------*/

        String save_flag = appsDefaults.getToken("mpe_save_gif");

        if ("save".equalsIgnoreCase(save_flag)) {
            String gif_dir = appsDefaults.getToken("mpe_gif_dir");
            String ftype = appsDefaults.getToken("mpe_gif_id");
            String fnamgif;
            if ((ftype == null) || ftype.isEmpty()) {
                fnamgif = String.format("%s/%sz.gif", gif_dir, cdate);
            } else {
                fnamgif = String.format("%s/%s%sz.gif", gif_dir, ftype, cdate);
            }

            mpegui_save_image(screenShot, "gif", fnamgif);
        } else {
            Activator.statusHandler.handle(Priority.VERBOSE,
                    "gif file not saved");
        }

        /*--------------------------------------*/
        /* generate and save jpeg image */
        /*--------------------------------------*/

        save_flag = appsDefaults.getToken("mpe_save_jpeg");

        if ("save".equalsIgnoreCase(save_flag)) {
            String jpeg_dir = appsDefaults.getToken("mpe_jpeg_dir");
            String ftype = appsDefaults.getToken("mpe_jpeg_id");
            String fnamjpeg;
            if ((ftype == null) || ftype.isEmpty()) {
                fnamjpeg = String.format("%s/%sz.jpeg", jpeg_dir, cdate);
            } else {
                fnamjpeg = String.format("%s/%s%sz.jpeg", jpeg_dir, ftype,
                        cdate);
            }

            mpegui_save_image(screenShot, "jpeg", fnamjpeg);
        } else {
            Activator.statusHandler.handle(Priority.VERBOSE,
                    "jpeg file not saved \n");
        }

        /*--------------------------------------*/
        /* generate and save netCDF file */
        /*--------------------------------------*/

        save_flag = appsDefaults.getToken("mpe_save_netcdf");

        if ("save".equalsIgnoreCase(save_flag)) {
            // TODO: Implement? There was lots of commented out code in
            // MPEDisplayManager and it did nothing
        } else {
            Activator.statusHandler.handle(Priority.VERBOSE,
                    "netCDF file not saved");
        }

        /*--------------------------------------*/
        /* generate and save grib file */
        /*--------------------------------------*/

        save_flag = appsDefaults.getToken("mpe_save_grib");

        if ("save".equalsIgnoreCase(save_flag)) {
            String ftype = appsDefaults.getToken("mpe_grib_id");
            String fnamgrib;
            if ((ftype == null) || ftype.isEmpty()) {
                fnamgrib = String.format("%sz.grib", cdate);
            } else {
                fnamgrib = String.format("%s%sz.grib", ftype, cdate);
            }

            String dirname = appsDefaults.getToken("mpe_grib_dir");

            Activator.statusHandler.handle(Priority.VERBOSE, String.format(
                    "Saving grib encoded file in %s/%s\n", dirname, fnamgrib));
            MPEProcessGrib mpgr = new MPEProcessGrib();
            mpgr.saveGrib(fileNameXmrg, fnamgrib);
        } else {
            Activator.statusHandler.handle(Priority.VERBOSE,
                    "grib encoded file not saved \n");
        }

        /* Check if the RFC Bias needs to be sent across the WAN. */
        boolean transmit_rfc_bias = appsDefaults.getBoolean("mpe_transmit_bias",
                true);
        boolean transmit_bias_on_save = appsDefaults
                .getBoolean("transmit_bias_on_save", true);
        if (transmit_rfc_bias && transmit_bias_on_save) {

            Date currentDate = MPEDisplayManager.getCurrent()
                    .getCurrentEditDate();
            SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMddHH");
            String transmitDate = formatter.format(currentDate);
            String scriptDir = appsDefaults.getToken("pproc_bin");
            String scriptName = "transmit_rfc_bias";
            ProcessBuilder pb = new ProcessBuilder(scriptDir + "/" + scriptName,
                    transmitDate);
            try {
                pb.start();
            } catch (IOException e) {
                e.printStackTrace();
            }

        }
        File qpePolyFile = PolygonEditManager.getHourlyEditFile(bestEstField,
                editDate);
        if (bestEstField.getFieldName()
                .equals(DisplayFieldData.Xmrg.getFieldName())) {
            if (qpePolyFile.exists()) {
                qpePolyFile.delete();
            }
        }
        MPEDisplayManager.getCurrent().setSavedData(true);
        MPEDisplayManager.getCurrent().getDisplayedFieldResource()
                .resourceChanged(ChangeType.DATA_UPDATE, editDate);
    }

    private static void mpegui_save_image(BufferedImage bi, String format,
            String path) {
        Activator.statusHandler.handle(Priority.VERBOSE,
                String.format("Saving %s file in %s\n", format, path));

        Iterator<ImageWriter> iter = ImageIO
                .getImageWritersByFormatName(format);
        ImageWriter writer = iter.next();

        try {
            writer.setOutput(new FileImageOutputStream(new File(path)));
            writer.write(bi);
        } catch (IOException e) {
            Activator.statusHandler.error("Error creating file ", e);
        }
    }

    private static void update_rwr_save(String rfc, Date dt, String fldtype) {

        String asave = "F";
        String drpr = "F";

        // RWA: this flag doesn't appear to be set anywhere
        // if ( applyprecip_flag == 1 )
        // {
        // drpr = "T";
        // }

        /* Build the obstime time string. */
        String datetime_obs_xmrg = MPEDateFormatter.format_obs(dt);

        /*
         * Build the where clause. This can be used for both the select and the
         * update.
         */
        String where = String.format("WHERE id.rfc='%s' AND id.obstime='%s'",
                rfc, datetime_obs_xmrg);

        /* Get the record to update from the RWResult table. */
        List<Rwresult> pRWResultHead = IHFSDbGenerated.GetRWResult(where);

        if (pRWResultHead.size() == 0) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    String.format(
                            "In routine 'update_rwr_save': Could not select a record from the RWResult table for query '%s'.\n",
                            where));
        } else {
            Rwresult pRWResultNode = pRWResultHead.get(0);

            /* Update the elements in the RWResult node. */

            fldtype = checkAndModifyMapxFieldType(fldtype);

            pRWResultNode.setMapxFieldType(fldtype.toLowerCase());
            pRWResultNode.setAutoSave(asave);
            pRWResultNode.setDrawPrecip(drpr);
            pRWResultNode
                    .setLastSaveTime(SimulatedTime.getSystemTime().getTime());

            /* Update the record in the database. */
            int status = IHFSDbGenerated.UpdateRWResult(pRWResultNode);

            if (status == -1) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        String.format(
                                "In routine 'update_rwr_save': could not update record in RWResult for query '%s'.",
                                where));
            }

            /*
             * Free the memory used by the linked list of RWResult structures.
             */
            pRWResultHead.clear();
        }

    }

    private static String checkAndModifyMapxFieldType(String fieldType) {

        // This method changes fieldType to lowercase.
        // It also shortens fieldTypes as needed to fit into the mapx_field_type
        // column in the RWResult table.
        // Note: the mapx_field_type column is informational only. It is not
        // used by the code
        // other than reading and writing from and to the database.

        String newFieldType = null;
        String lowerCaseFieldType = fieldType.toLowerCase();

        final Map<String, String> conversionTable = new HashMap<String, String>();

        conversionTable.put("localfield1", "localfld1");
        conversionTable.put("localfield2", "localfld2");
        conversionTable.put("localfield3", "localfld3");

        conversionTable.put("avgrdmosaic", "avgrdmos");
        conversionTable.put("maxrdmosaic", "maxrdmos");

        newFieldType = conversionTable.get(lowerCaseFieldType);
        if (newFieldType == null) {
            newFieldType = lowerCaseFieldType;
        }
        return newFieldType;
    }
}
