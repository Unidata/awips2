package gov.noaa.nws.ncep.viz.rsc.ntrans.rsc;

import gov.noaa.nws.ncep.common.dataplugin.ntrans.NtransRecord;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.Command;
import gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.NcCGM;
import gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.NcText;
import gov.noaa.nws.ncep.viz.ui.display.NCNonMapDescriptor;

import java.io.ByteArrayInputStream;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.HashMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;

/**
 * NtransResource - Resource for Display of NTRANS Metafiles.
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 21 Nov 2012   838        B. Hebbard  Initial creation.
 * 25 Apr 2013   838        G. Hull     add request constraint to the query for the cycle time  
 * 30 Apr 2013   838        B. Hebbard  IOC version (for OB13.4.1)
 * 30 May 2013   838        B. Hebbard  Update for compatibility with changes by RTS in OB13.3.1
 *                                      [ DataStoreFactory.getDataStore(...) parameter ]
 * 29 Jul 2014   R4279      B. Hebbard  (TTR 1046) Add call to processNewRscDataList() in initResource()
 *                                      (instead of waiting for ANCR.paintInternal() to do it) so
 *                                      long CGM retrieval and parsing is done by the InitJob, and thus
 *                                      (1) user sees "Initializing..." and (2) GUI doesn't lock up
 * 29 Aug 2014              B. Hebbard  Remove time string and "/" separator from legend
 * 12 Sep 2014              B. Hebbard  Refactor to avoid regenerating paintables from CGM on each paint.
 * 
 * </pre>
 * 
 * @author bhebbard
 * @version 1.0
 */
public class NtransResource extends
        AbstractNatlCntrsResource<NtransResourceData, NCNonMapDescriptor>
        implements INatlCntrsResource {

    private NtransResourceData ntransResourceData;

    private String legendStr = "NTRANS "; // init so not-null

    // TODO static better??
    private final Log logger = LogFactory.getLog(this.getClass());

    private class FrameData extends AbstractFrameData {

        // FrameData holds information for a single frame (time).

        NtransRecord ntransRecord; // the metadata record (PDO)

        NcCGM cgmImage; // "jcgm" (connected Java objects) representation of the
                        // CGM image

        PaintableImage paintableImage; // AWIPS graphics elements of the image;
                                       // compiled and ready to paint

        public FrameData(DataTime frameTime, int timeInt) {
            super(frameTime, timeInt);
        }

        public boolean updateFrameData(IRscDataObject rscDataObj) {
            if (!(rscDataObj instanceof DfltRecordRscDataObj)) {
                logger.error("NtransResource.updateFrameData expecting DfltRecordRscDataObj "
                        + " instead of: " + rscDataObj.getClass().getName());
                return false;
            }
            // TODO : check that the cycle times match.

            if (ntransRecord != null) {
                System.out
                        .println("adding record to frame that has already been populated");
                // Add code here to check if the new data is a better time
                // match. If not then discard, and if so dispose of the
                // existing data and process the new record. [from GH]
                return false;
            }

            // Get PDO from the given RDO

            DfltRecordRscDataObj ntransRDO = (DfltRecordRscDataObj) rscDataObj;
            ntransRecord = (NtransRecord) ntransRDO.getPDO();

            // Get binary CGM image data from data store

            long t0 = System.currentTimeMillis();
            byte[] imageBytes = getBinaryCgmFromNtransRecord(ntransRecord);
            long t1 = System.currentTimeMillis();
            if (imageBytes == null) {
                logger.error("NtransResource.updateFrameData imageBytes from NtransRecord is null ");
                return false;
            }

            // Fix endianess if needed

            boolean flipped = false;
            if (imageBytes[0] == 96) { // TODO clean up and/or move to decoder?
                imageBytes = shuffleByteArray(imageBytes);
                flipped = true;
            }

            // Construct "jcgm" (Java objects) CGM representation of the image
            // from the binary CGM

            cgmImage = new NcCGM();
            InputStream is = new ByteArrayInputStream(imageBytes);
            DataInput di = new DataInputStream(is);

            try {
                long t2 = System.currentTimeMillis();
                cgmImage.read(di); // <-- Voom!
                long t3 = System.currentTimeMillis();
                logger.info("CGM image " + ntransRecord.getImageByteCount()
                        + " bytes retrieved from HDF5 in " + (t1 - t0) + " ms"
                        + " and parsed in " + (t3 - t2) + " ms");
            } catch (Exception e) {
                logger.info("CGM image " + ntransRecord.getImageByteCount()
                        + " bytes retrieved from HDF5 in " + (t1 - t0) + " ms");
                logger.error("EXCEPTION occurred interpreting CGM"
                        + " for metafile " + ntransRecord.getMetafileName()
                        + " product " + ntransRecord.getProductName());
                e.printStackTrace();
            }

            // Endianess revisited

            if (flipped) { // if we shuffled the bytes before parsing...
                flipStrings(cgmImage); // ...then unshuffle the strings
                                       // (now that we know where they are)
            }

            // TODO Add optional (cool) debug dump of CGM representation
            // cgmImage.showCGMCommands();

            return true;
        }

        private byte[] shuffleByteArray(byte[] image) {
            // Flip every even byte with its odd sibling (endianess reversal)
            byte[] returnArray = new byte[image.length];
            for (int i = 0; i < image.length; i = i + 2) {
                returnArray[i] = image[i + 1];
                returnArray[i + 1] = image[i];
            }
            return returnArray;
        }

        private void flipStrings(NcCGM cgm) {
            for (Command c : cgm.getCommands()) {
                if (c instanceof NcText) {
                    NcText nct = (NcText) c;
                    nct.flipString();
                }
            }
        }

        private byte[] getBinaryCgmFromNtransRecord(NtransRecord nr) {

            // Given the NcscatRecord, locate the associated HDF5 data...

            File location = HDF5Util.findHDF5Location(nr);

            // TODO... Investigate: Why is the following statement needed?
            // Starting in OB13.5.3, the PDO (nr) has a non-null, but bogus,
            // value in its dataURI field at this point (and earlier,
            // as soon as it is deserialized after return from the metadata
            // query). nr.getDataURI() below will get this bad value, leading
            // to failure on the ds.retrieve(...). Instead we force it to
            // synthesize the dataURI -- which getDataURI() does correctly --
            // by setting the field to null first. But why is this happening,
            // and why only in OB13.5.3, and why only for some resources...?
            // (bh)
            // (see also NCSCAT resource)
            nr.setDataURI(null); // force it to construct one

            String group = nr.getDataURI();
            // String uri = nr.getDataURI();
            String dataset = "NTRANS";

            // @formatter:off
            /*
             * // get filename and directory for IDataStore String dir =
             * nr.getHDFPathProvider().getHDFPath(nr.getPluginName(), nr);
             * String filename = nr.getHDFPathProvider().getHDFFileName(
             * nr.getPluginName(), nr); File file = new File(dir, filename);
             */
            // @formatter:on

            // ...and retrieve it

            IDataStore ds = DataStoreFactory.getDataStore(location);
            IDataRecord dr = null;
            // IDataRecord[] dr;
            try {
                dr = ds.retrieve(group, dataset, Request.ALL);
                // dr = ds.retrieve(uri);
            } catch (FileNotFoundException e) {
                logger.error("[EXCEPTION occurred retrieving CGM"
                        + " for metafile " + nr.getMetafileName() + " product "
                        + nr.getProductName() + "]");
                e.printStackTrace();
                return null;
            } catch (StorageException e) {
                logger.error("[EXCEPTION occurred retrieving CGM"
                        + " for metafile " + nr.getMetafileName() + " product "
                        + nr.getProductName() + "]");
                e.printStackTrace();
                return null;
            }

            // return (byte[]) dr[0].getDataObject();
            return (byte[]) dr.getDataObject();
        }

        public void dispose() {
            if (paintableImage != null) {
                paintableImage.dispose();
                paintableImage = null;
            }
            super.dispose();
        }
    }

    // ------------------------------------------------------------

    /**
     * Create an NTRANS Metafile display resource.
     * 
     * @throws VizException
     */
    public NtransResource(NtransResourceData resourceData,
            LoadProperties loadProperties) throws VizException {
        super(resourceData, loadProperties);
        ntransResourceData = (NtransResourceData) resourceData;

        // Set the legend from the metafileName and productName.
        // NOTE: This assumes that the request type of EQUALS
        // (i.e. only one kind of metafileName and productName) (??) [from GH]
        //
        if (ntransResourceData.getMetadataMap().containsKey("metafileName")
                && ntransResourceData.getMetadataMap().containsKey(
                        "productName")) {
            legendStr = " "
                    + ntransResourceData.getMetadataMap().get("metafileName")
                            .getConstraintValue()
                    + "  "
                    // + " / "
                    + ntransResourceData.getMetadataMap().get("productName")
                            .getConstraintValue();
        }
    }

    protected AbstractFrameData createNewFrame(DataTime frameTime, int timeInt) {
        return (AbstractFrameData) new FrameData(frameTime, timeInt);
    }

    // Query all the data in the DB matching the request constraints (modelName,
    // metaFile, and productName) and also match the selected cycle time.
    //
    public void initResource(IGraphicsTarget grphTarget) throws VizException {
        // Set initial display values from resource attributes (as if after
        // modification)

        // resourceAttrsModified(); // none now; possible future enhancement

        ResourceName rscName = getResourceData().getResourceName();

        // Set the constraints for the query
        String[] dts = rscName.getCycleTime().toString().split(" ");
        String cycleTimeStr = dts[0] + " "
                + dts[1].substring(0, dts[1].length() - 2);

        HashMap<String, RequestConstraint> reqConstraintsMap = new HashMap<String, RequestConstraint>(
                ntransResourceData.getMetadataMap());

        RequestConstraint timeConstraint = new RequestConstraint(cycleTimeStr);
        reqConstraintsMap.put("dataTime.refTime", timeConstraint);

        LayerProperty prop = new LayerProperty();
        prop.setDesiredProduct(ResourceType.PLAN_VIEW);
        prop.setEntryQueryParameters(reqConstraintsMap, false);
        prop.setNumberOfImages(15000); // TODO: max # records ??
                                       // Should we cap this ?
        String script = null;
        script = ScriptCreator.createScript(prop);

        if (script == null) {
            return;
        }

        long t0 = System.currentTimeMillis();
        Object[] pdoList = Connector.getInstance().connect(script, null, 60000);
        long t1 = System.currentTimeMillis();

        logger.info("Metadata records for " + this.newRscDataObjsQueue.size()
                + " images retrieved from DB in " + (t1 - t0) + " ms");

        for (Object pdo : pdoList) {
            for (IRscDataObject dataObject : processRecord(pdo)) {
                newRscDataObjsQueue.add(dataObject);
            }
        }

        // TODO -- why is this here? Still needed?
        setAllFramesAsPopulated();

        // Following is done in ANCR.paintInternal too, but want to get it done
        // on the init thread since it's time-consuming and (1) we want to show
        // the "Initializing..." pacifier message, and (2) not lock up the GUI
        // thread during loading. (Might want to consider doing this in ANCR.)
        if (!newRscDataObjsQueue.isEmpty()
        // || (!newFrameTimesList.isEmpty() && getDescriptor().isAutoUpdate())
        ) {
            processNewRscDataList();
        }
    }

    public void paintFrame(AbstractFrameData frameData, IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        // Are we "go" to paint?

        if (frameData == null || target == null || paintProps == null) {
            return;
        }
        FrameData fd = (FrameData) frameData;
        if (fd.ntransRecord == null) {
            // throw new VizException();
            return; // TODO why?
        }
        if (fd.paintableImage == null && fd.cgmImage == null) {
            // throw new VizException();
            return; // TODO why?
        }

        // If a ready-to-paint image has not yet been built for this frame, then
        // construct one from the (Java representation of the) CGM image

        final boolean discardCGMAfterPaintableImageBuilt = true;

        if (fd.paintableImage == null) {
            double scale = 1000.000 / (double) fd.ntransRecord.getImageSizeX();
            try {
                fd.paintableImage = new PaintableImage(fd.cgmImage, target,
                        paintProps, descriptor, scale);
                if (discardCGMAfterPaintableImageBuilt) {
                    fd.cgmImage = null;
                }
            } catch (Exception e) {
                logger.error("[EXCEPTION occurred constructing paintable image"
                        + " for metafile " + fd.ntransRecord.getMetafileName()
                        + " product " + fd.ntransRecord.getProductName() + "]");
                fd.paintableImage = null;
                fd.cgmImage = null; // don't keep trying on subsequent paints
                return;
            }
        }

        // Paint it

        fd.paintableImage.paint();
    }

    public void resourceAttrsModified() {
        // So far, none exist (possible future enhancement)
    }

    @Override
    public String getName() {
        FrameData fd = (FrameData) getCurrentFrame();
        if (fd == null || fd.getFrameTime() == null
                || (fd.paintableImage == null && fd.cgmImage == null)) {
            return legendStr + "-No Data";
        }
        return legendStr; // + " "
        // + NmapCommon.getTimeStringFromDataTime(fd.getFrameTime(), "/");
    }
}