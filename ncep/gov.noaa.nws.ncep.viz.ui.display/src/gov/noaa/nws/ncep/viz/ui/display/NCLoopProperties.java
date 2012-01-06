package gov.noaa.nws.ncep.viz.ui.display;

import com.raytheon.uf.viz.core.datastructure.LoopProperties;

public class NCLoopProperties extends LoopProperties {

    /** flag indicating current loop stop position */
    private boolean loopStopCurrent = true;
    
    /*
     * NCEP loop properties
     */
    /** Minimum NCEP frame time in ms */
    private int minNcepFrameTime = MIN_FRAME_TIME;
   
    /** Maximum NCEP frame time in ms */
    private int maxNcepFrameTime = MAX_FRAME_TIME;
    
    /** NCEP frame time increment in ms */
    private int ncepFrameStep = FRAME_STEP;
    
    /** maximum NCEP first frame dwell time in ms */
    private int maxNcepFirstFrameDwellTime = MAX_DWELL_TIME;
    
    /** maximum NCEP last frame dwell time in ms */
    private int maxNcepLastFrameDwellTime = MAX_DWELL_TIME;
   
    /** NCEP first frame dwell time increment in ms */
    private int ncepFirstFrameDwellStep = DWELL_STEP;
    
    /** NCEP last frame dwell time increment in ms */
    private int ncepLastFrameDwellStep = DWELL_STEP;
    
    
    /*
     * Set loop stop position
     */
    public void setLoopStopCurrent (boolean loopStop ) {
    	this.loopStopCurrent = loopStop;
    	
    }
    
    /*
     * Get loop stop position
     */
    public boolean getLoopStopCurrent () {
    	return loopStopCurrent;
    	
    } 
    
    /*
     * Set NCEP minimum frame time
     */
    public void setNcepMinFrameTime ( int frameTime ) {
    	minNcepFrameTime = frameTime ;
    }
    
    /*
     * Get NCEP minimum frame time
     */
    public int getNcepMinFrameTime () {
    	return minNcepFrameTime;
    }
    
    /*
     * Set NCEP maximum frame time
     */
    public void setNcepMaxFrameTime ( int frameTime ) {
    	maxNcepFrameTime = frameTime ;
    }
    
    /*
     * Get NCEP maximum frame time
     */
    public int getNcepMaxFrameTime () {
    	return maxNcepFrameTime;
    }
    
    /*
     * Set NCEP frame step
     */
    public void setNcepFrameStep ( int frameStep ) {
    	ncepFrameStep = frameStep ;
    }
    
    /*
     * Get NCEP frame step
     */
    public int getNcepFrameStep () {
    	return ncepFrameStep;
    }
    
    /*
     * Set NCEP first frame dwell step
     */
    public void setNcepFirstFrameDwellStep ( int frameDwellStep ) {
    	ncepFirstFrameDwellStep = frameDwellStep ;
    }
    
    /*
     * Get NCEP first frame dwell step
     */
    public int getNcepFirstFrameDwellStep () {
    	return ncepFirstFrameDwellStep;
    }

    /*
     * Set NCEP last frame dwell step
     */
    public void setNcepLastFrameDwellStep ( int frameDwellStep ) {
    	ncepLastFrameDwellStep = frameDwellStep ;
    }
    
    /*
     * Get NCEP last frame dwell step
     */
    public int getNcepLastFrameDwellStep () {
    	return ncepLastFrameDwellStep;
    }
    
    /*
     * Set NCEP maximum first frame time
     */
    public void setNcepMaxFirstFrameTime ( int frameTime ) {
    	maxNcepFirstFrameDwellTime = frameTime ;
    }
    
    /*
     * Get NCEP minimum first frame time
     */
    public int getNcepMaxFirstFrameTime () {
    	return maxNcepFirstFrameDwellTime;
    }
 
    /*
     * Set NCEP maximum last frame time
     */
    public void setNcepMaxLastFrameTime ( int frameTime ) {
    	maxNcepLastFrameDwellTime = frameTime ;
    }
    
    /*
     * Get NCEP minimum last frame time
     */
    public int getNcepMaxLastFrameTime () {
    	return maxNcepLastFrameDwellTime;
    }
}
