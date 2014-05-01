package gov.noaa.nws.ncep.edex.common.dataRecords;

import java.util.Arrays;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import
com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    	Description
 * ------------ ----------  ----------- 	--------------------------
 * Apr 6, 2010  164          M. Li      	Modified from NcFloatDataRecord
 * Jun15, 2010	164			mgamazaychikov	Cleaned up, improved reduce method,
 * 											implemented cloneInternal, getSizeInBytes
 * </pre>
 *
 * @author mli
 * @version 1
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class NcFloatDataRecord extends AbstractStorageRecord{

    @DynamicSerializeElement
    @XmlAttribute
    protected float[]xdata;

    @DynamicSerializeElement
    @XmlAttribute
    protected float[]ydata;
    
    @DynamicSerializeElement
    @XmlAttribute
    boolean vector = false;

	public NcFloatDataRecord() {

    }

    public NcFloatDataRecord(float[] aFloatData) {
    	setXdata(aFloatData);
        this.dimension = 1;
        setSizes(new long[] { aFloatData.length});
    }
    
    /**
     * Constructor
     * 
     * @param xdata
     *            the float data as 1d array
     * @param dimension
     *            the dimension of the data
     * @param sizes
     *            the length of each dimension
     */
    public NcFloatDataRecord(float[] aFloatData, long[] aSizes) {  	
    	this.xdata = aFloatData;
        this.dimension = 1;
        this.sizes = aSizes;
    }
   
    /**
     * @return the xdata
     */
    public float[] getXdata() {
        return xdata;
    }

    /**
     * @param xdata
     *            the xdata to set
     */
    public void setXdata(float[] aFloatData) {
        this.xdata = aFloatData;
    }

    public float[] getYdata() {
        return ydata;
    }

    /**
     * @param ydata
     *            the ydata to set
     */
    public void setYdata(float[] aFloatData) {
        this.ydata = aFloatData;
    }
    
    /*
     * (non-Javadoc)
     *
     * @see
com.raytheon.edex.storage.records.AbstractDataRecord#getDataObject()
     */
    @Override
    public Object getDataObject() {
        return this.xdata;
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.edex.storage.records.IDataRecord#validateDataSet()
     */
    public boolean validateDataSet() {

        long size = 1;

        for (int i = 0; i < this.dimension; i++) {
            size *= this.sizes[i];
        }

        if (size == this.xdata.length) {
            return true;
        } else {
            return false;
        }

    }

    /**
     * Override toString method to print dimensions
     */
    @Override
    public String toString() {
        return "[dims,data size]=[" + this.dimension + ","
                + this.xdata.length + "]";
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.edex.storage.records.IDataRecord#reduce(int[])
     */
    @Override
    public void reduce(int[] indices) {
    	if ( dimension == 1 ) {
    		float[] reducedxData = new float[indices.length];
            for (int i = 0; i < reducedxData.length; i++) {
                if (indices[i] >= 0) {
                    reducedxData[i] = xdata[indices[i]];
                } else {
                    reducedxData[i] = -9999;
                }
            }
            this.xdata = reducedxData;
            setDimension(1);
            setSizes(new long[] { indices.length });
    	}
    	if ( dimension == 2 ) {
    		float[] reducedxData = new float[indices.length];
    		float[] reducedyData = new float[indices.length];
            for (int i = 0; i < reducedxData.length; i++) {
                if (indices[i] >= 0) {
                    reducedxData[i] = xdata[indices[i]];
                    reducedyData[i] = ydata[indices[i]];
                } else {
                    reducedxData[i] = -9999;
                    reducedyData[i] = -9999;
                }
            }
            this.xdata = reducedxData;
            this.ydata = reducedyData;
            setDimension(2);
            setSizes(new long[] { indices.length, indices.length });
    	}	
    }
    
    public boolean isVector() {
		return vector;
	}

	public void setVector(boolean vector) {
		this.vector = vector;
	}

	@Override
	protected AbstractStorageRecord cloneInternal() {
		NcFloatDataRecord record = new NcFloatDataRecord();		
        if (xdata != null) {
            record.xdata = Arrays.copyOf(xdata, xdata.length);
        }
        if (ydata != null) {
            record.ydata = Arrays.copyOf(ydata, ydata.length);
        }
        record.dimension = dimension;
        record.sizes = sizes;
        return record;
	}

	@Override
	public int getSizeInBytes() {
		if ( xdata ==  null && ydata == null) {
			return 0;
		}
		else if ( xdata ==  null && ydata != null) {
			return ydata.length * 4;
		}
		else if ( xdata !=  null && ydata == null) {
			return xdata.length * 4;
		}
		else {
			return (xdata.length + ydata.length) * 4;
		}
	}
}