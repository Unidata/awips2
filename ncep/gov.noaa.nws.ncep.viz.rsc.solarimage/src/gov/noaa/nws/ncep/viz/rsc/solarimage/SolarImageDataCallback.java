package gov.noaa.nws.ncep.viz.rsc.solarimage;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.HeaderData;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.ImageData;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.SolarImageUtil;

import java.nio.FloatBuffer;

import nom.tam.fits.BasicHDU;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * dataCallback for SolarImage when LINEAR scale
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 * 01/22/2013    958         qzhou     Initial Creation.
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */
public class SolarImageDataCallback implements IColorMapDataRetrievalCallback {

    protected SolarImageRecord record;

    private final HeaderData headerData;

    protected ImageData imgData;

    private int imageNx = 0;

    private int imageNy = 0;

    private int cylindrical = 0;

    private int imgDiff = 0;

    /**
     * @param dataURI
     * @throws VizException
     */
    public SolarImageDataCallback(SolarImageRecord record) throws VizException {
        this.record = record;
        BasicHDU recordHDU = getDataFromHDF5();
        this.headerData = new HeaderData(recordHDU);
        this.imageNx = headerData.getNx();
        this.imageNy = headerData.getNy();
        this.imgData = new ImageData(imageNx, imageNy, headerData.getBitpix(),
                headerData.getBscale(), headerData.getBzero(),
                recordHDU.getKernel());

    }

    @Override
    public ColorMapData getColorMapData() throws VizException {

        if (imgData == null) {
            this.imgData = new ImageData(imageNx, imageNy,
                    headerData.getBitpix(), headerData.getBscale(),
                    headerData.getBzero(), getDataFromHDF5().getKernel());
            this.imageNx = imgData.getNx();
            this.imageNy = imgData.getNy();
        }

        int[] dimensions = new int[] { imgData.getNx(), imgData.getNy() };

        FloatBuffer buffer = FloatBuffer.wrap(imgData.getImageValues());

        return new ColorMapData(buffer, dimensions, ColorMapDataType.FLOAT);
    }

    public BasicHDU getDataFromHDF5() throws VizException {
        BasicHDU recordHDU = SolarImageUtil.getImageHDU(record);
        record.setRawData(null);
        return recordHDU;
    }

    public double getOriginalValue(double val) {
        return val;
    }

    public ImageData getImageData() {
        return imgData;
    }

    public void setImageData(ImageData imgData) {
        this.imgData = imgData;
        if (imgData != null) {
            this.imageNx = imgData.getNx();
            this.imageNy = imgData.getNy();
        }
    }

    public SolarImageRecord getRecord() {
        return record;
    }

    public int getImageNx() {
        return imageNx;
    }

    public void setImageNx(int imageNx) {
        this.imageNx = imageNx;
    }

    public int getImageNy() {
        return imageNy;
    }

    public void setImageNy(int imageNy) {
        this.imageNy = imageNy;
    }

    public HeaderData getHeaderData() {
        return headerData;
    }

    public int getCylindrical() {
        return cylindrical;
    }

    public void setCylindrical(int cylindrical) {
        this.cylindrical = cylindrical;
    }

    public int getImgDiff() {
        return imgDiff;
    }

    public void setImgDiff(int imgDiff) {
        this.imgDiff = imgDiff;

    }

    @Override
    public int hashCode() {

        final int prime = 31;
        int result = 1;

        result = prime * result + cylindrical
                + this.getClass().getCanonicalName().hashCode();

        result = prime * result + imgDiff
                + this.getClass().getCanonicalName().hashCode();

        result = prime * result
                + ((this.imgData == null) ? 0 : this.imgData.hashCode());

        result = prime * result
                + ((this.record == null) ? 0 : this.record.hashCode());

        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SolarImageDataCallback other = (SolarImageDataCallback) obj;
        if (this.getHeaderData() != other.getHeaderData()) {
            return false;
        }
        if (this.record == null) {
            if (other.getRecord() != null)
                return false;
        } else if (!this.record.equals(other.getRecord()))
            return false;
        if (this.getImageData() == null) {
            if (other.getImageData() != null)
                return false;
        } else if (this.getImageData() != other.getImageData()) {
            return false;
        }
        if (this.getCylindrical() != other.getCylindrical()) {
            return false;
        }
        if (this.getImgDiff() != other.getImgDiff()) {
            return false;
        }
        return true;
    }

}
