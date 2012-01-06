/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.GfaRules
 * 
 * July 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaRules.*;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.dom4j.Node;

import com.vividsolutions.jts.geom.Polygon;

/**
 * Applying worst case attributes to GFA airmets/outlooks from the selected snapshots,
 * including issue status, IFR types, flight top/bottom, FZLVL top/bottom, 
 * frequency, severity.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/11			?		J. Wu		Initial creation
  * </pre>
 * 
 * @author J. Wu
 * @version 1
 */

public class GfaWorstAttr {
		
    /*
     *  The type of the forecast cycle - on-time or off-time
     *  "on-time" cycles - 03/09/15/21 for winter and 02/08/14/20 for summer.
     *                     snapshot sequence runs 00 to 06 for FROM line
     *                     snapshot sequence include 06, 09, 12 for OUTLOOK
     *  "OffTime" cycles - 00/06/12/18 for winter and 05/11/17/23 for summer. 
     *                     snapshot sequence runs 00 to 03 for FROM line
     *                     snapshot sequence include 03, 06, 09 for OUTLOOK
     *   Note: special hours may be issued for FROM line, but NOT for OUTLOOK.
     */	
	private static final int[] onTimeCycles = new int[] { 3, 9, 15, 21,  2, 8, 14, 20};
	private static final int[] offTimeCycles = new int[] { 0, 6, 12, 18, 5, 11, 17, 23 };
    
	public GfaWorstAttr() {		
	}
	
	/**
	 * Applies worst case attributes from the selected snapshots to clipped smears.
	 * 
	 * @param smear		Original smear before clipped 
	 * @param list		List of GFAs to be processed.
	 * @param snapshots	List of original snapshots which were used to create the smear
	 * @param canceled	List of CAN snapshots which were removed.
	 */
	public static void apply( Gfa smear, ArrayList<Gfa> clipped, ArrayList<Gfa> snapshots,
							  ArrayList<Gfa> canceled ) {
        		
		//Skip FZLVL and those hazards without snapshots.
		if ( smear.getGfaHazard().equals( "FZLVL" ) || 
			 ( snapshots.size() + canceled.size() == 0 ) ) {
			return;
		}
		

	    //Find the type of the forecast cycle - on-time or off-time
	 	int baseTime = PgenCycleTool.getCycleHour();
		boolean onTime = false;
		for ( int ii = 0; ii < onTimeCycles.length; ii++ ) {
			if ( baseTime == onTimeCycles[ ii ] ) {
				onTime = true;
				break;
			}
		}
		
		/*
		 *  Find the type of the smear (AIRMET or OUTLOOK) and determine
		 *  the starting and ending time for snapshot sequence.
		 */
		int startTime;
		int endTime;
		
		if ( smear.isOutlook() ) {
		    if ( onTime ) {
		        startTime = 360;
			    endTime = 720;
		    }
		    else {
		        startTime = 180;
		        endTime = 540;	        
		    }       						
		}
		else { //Airmet
		    startTime = 0;
		    if ( onTime ) {
			    endTime = 360;
		    }
		    else {
		        endTime = 180;	        
		    }
		}
		
		/*
		 *  Now determine the worst attributes for each clipped smear.
		 */
		for ( Gfa gfa : clipped ) {
		
			ArrayList<SnapshotGroup> ssGrp = getSnapshotGroup( snapshots, gfa );

			//Find the snapshot sequence based on cycle and forecast hour. 
			int start_indx = -1;
			int end_indx = -1;
            int nhour = ssGrp.size();
            
			for ( int jj = 0; jj < nhour; jj++ ) {
			    
			    int fcst_min = ssGrp.get( jj ).hour * 60 + ssGrp.get( jj ).minute;
			    
			    if ( start_indx < 0 && fcst_min >= startTime ) {
			         start_indx = jj;
			         end_indx = jj;	    
			    } 
			    
			    if ( fcst_min <= endTime ) {
			         end_indx = jj;	    
			    }
			    
			}
			
			/*
			 *  Adjust the selection of snapshots based on the rule:
			 *  The starting snapshot should be an X or an O followed by an X.
			 *  The end snapshot should be an X or an O followed by an X, if we
			 *  look the snapshot sequence in reverse order.
			 */	
			for ( int jj = start_indx;  jj <= end_indx; jj++ ) {
			    if ( ssGrp.get( jj ).type == SnapshotType.X ) {
			        break;		
			    }
			    else {
			        if ( (jj+1) < nhour  && ssGrp.get( jj+1 ).type ==  SnapshotType.X ) {
				        break;
				    }
				    else {
				        if ( ( start_indx + 1 ) < nhour ) {
				            start_indx++;
				        }
				    } 
			    }	         
			}

			for ( int jj = end_indx;  jj >= start_indx; jj-- ) {
			    if ( ssGrp.get( jj ).type == SnapshotType.X ) {
			        break;		
			    }
			    else {
			        if ( (jj-1) >= 0  && ssGrp.get( jj-1 ).type ==  SnapshotType.X ) {
				        break;
				    }
				    else {
				        end_indx--;
				    } 
			    }	         
			}
			
			//Skip if no snapshots selected.
			if ( start_indx < 0 )  {
			    continue;
			}
						
			//Get the worst issue status from the selected snapshots.
			ArrayList<Gfa> qualifiedSS = new ArrayList<Gfa>();
			for ( int jj = start_indx;  jj <= end_indx; jj++ ) {		            
								
				int nsnap = ssGrp.get( jj ).getSnapshotInfo().size();				
				for ( int kk = 0;  kk < nsnap; kk++ ) {
					
					if ( ssGrp.get( jj ).type ==  SnapshotType.X && 
						 ssGrp.get( jj ).getSnapshotInfo().get( kk ).type == SnapshotType.O ) {
					    continue;   
					}
					
					qualifiedSS.add( snapshots.get( ssGrp.get( jj ).start + kk ) );
				}				    
		    }
			
			
		 	//Need to consider those CANceled snapshots for worst issue type.
			ArrayList<Gfa> worstIssueSS = new ArrayList<Gfa>( qualifiedSS );			
			if ( canceled != null && canceled.size() > 0 ) {
				worstIssueSS.addAll( canceled );
			}
			
			String worstIssue = getGfaWorstIssueType( worstIssueSS );
			if ( worstIssue.length() > 0 ) {
				gfa.setGfaIssueType( worstIssue );
			}
			
			/*
	         *  Get the worst cases for top/bottom flight levels, and the top/bottom 
	         *  freezing levels.
			 */
			HashMap<String, String> topBotValues = GfaFormat.findGfaTopBots( qualifiedSS );
			HashMap<String, String> existingValues = gfa.getGfaValues();
			for ( String skey : topBotValues.keySet() )	{
            	String value = topBotValues.get( skey );
            	if ( value != null && value.length() > 0 ) {
            		existingValues.put( skey, value );
            	}
            }
						
	        //Get the worst cases for types (IFR).		
            String combineTypes = GfaFormat.findGfaSubTypes( qualifiedSS );
            if ( combineTypes != null && combineTypes.length() > 0 ) {
            	gfa.setGfaType( combineTypes );
            }
            
	        //Get the worst cases for frequency.		
            String worstFreq = getGfaWorstFrequency( qualifiedSS );       
            if (  worstFreq!= null && worstFreq.length() > 0 ) {
            	gfa.setGfaValue( Gfa.FREQUENCY, worstFreq );
            }
			
	        //Get the worst cases for severity ( to do ===== > ).					
			
		}

	}


	/**
	 *  A Class to hold a snapshot's information.
	 */
	class SnapshotInfo {
	    
		SnapshotType	type;		/* snapshot's type - X_SNAPSHOT/O_SNAPSHOT */
	    int				hour;		/* hour part of snapshot's forecast hour */
	    int				minute;		/* minute part of snapshot's forecast hour */
	    int				dvlpg_hr;	/* snapshot's hour for development wording */
	    int				endg_hr;	/* snapshot's hour for ending wording */
	       
		public SnapshotInfo( SnapshotType type, int hour, int minute, int dvlpg_hr, int endg_gr ) {
			this.type = type;
			this.hour = hour;
			this.minute = minute;
			this.dvlpg_hr = dvlpg_hr;
			this.endg_hr = endg_gr;
		}
		
	}

	/**
	 *  A Class to hold a snapshot group's information.
	 */
	class SnapshotGroup {
	    SnapshotType	type;		/* group's type at this forecast time: 	
 	 									X-SNAPSHOTS - at least one ss is X	
										O_SNAPSHOTS - all SS is O_SNAPSHOTS	
	     							*/
	    int				hour;		/* hour part of the forecast hour */
	    int				minute;		/* minute part of the forecast hour */
	    int		    	dvlpg_hr;	/* group's hour for development wording */
	    int		    	endg_hr;	/* group's hour for ending wording 	*/
//	    int				nsnap;		/* number of ss with this forecast hour	= snapshotInfo.size() */
	    int				start;		/* start index in the ss array 		*/
	    
	    ArrayList<SnapshotInfo>	snapshotInfo;		/* info for each snapshot at this hour	*/
	    	       
		public SnapshotGroup( SnapshotType type, int hour, int minute, int dvlpg_hr, int endg_gr,
							  int start, ArrayList<SnapshotInfo> snapshotInfo ) {
			this.type = type;
			this.hour = hour;
			this.minute = minute;
			this.dvlpg_hr = dvlpg_hr;
			this.endg_hr = endg_gr;
			this.start = start;
            
			this.snapshotInfo = new ArrayList<SnapshotInfo>();
			if ( snapshotInfo != null ) {
				this.snapshotInfo.addAll( snapshotInfo );
			}
			
		}

		/**
		 * @return the snapshotInfo
		 */
		public ArrayList<SnapshotInfo> getSnapshotInfo() {
			return snapshotInfo;
		}

		/**
		 * @param type the type to set
		 */
		public void setType( SnapshotType type ) {
			this.type = type;
		}
		
		/**
		 * @param endg_hr the endg_hr to set
		 */
		public void setDvlpg_hr( int dvlpg_hr ) {
			this.dvlpg_hr = dvlpg_hr;
		}
		
		/**
		 * @param endg_hr the endg_hr to set
		 */
		public void setEndg_hr( int endg_hr ) {
			this.endg_hr = endg_hr;
		}

	}
	
	/* 
	 * Groups snapshots at each unique forecast hour and determine each group's
	 * attributes from the snapshots within the group.
	 * 
	 * @param snapshots	
	 * @param clipped	clipped smear
	 * 
     */
	private static ArrayList<SnapshotGroup> getSnapshotGroup( ArrayList<Gfa> snapshots, Gfa clipped ) {
		
	    HashMap<String, SnapshotGroup>  ssgrpMap = new HashMap<String, SnapshotGroup>();
		GfaWorstAttr wattr = new GfaWorstAttr();
		
	    int ii = 0;
		for ( Gfa gfa : snapshots ) {
						
			SnapshotInfo ssinfo = wattr.createSnapshotInfo( gfa, clipped );
			SnapshotGroup sgrp = ssgrpMap.get( gfa.getGfaFcstHr() );
			
			if ( sgrp != null ) {  //add into an existing group
				
				sgrp.getSnapshotInfo().add( ssinfo );
				
				if ( ssinfo.type == SnapshotType.X ) {
					sgrp.setType( ssinfo.type );
					sgrp.setDvlpg_hr( ssinfo.dvlpg_hr );
					sgrp.setEndg_hr( ssinfo.endg_hr );
				}
			}
			else { //create a new group
				sgrp = wattr.createSnapshotGroup( ssinfo, ii );
				ssgrpMap.put( gfa.getGfaFcstHr(), sgrp );
			}
			
			ii++;
			
		}
				
		return new ArrayList<SnapshotGroup>( ssgrpMap.values() );
		
	}

	
	/* 
	 * Creates a new SnapshotInfo for a snapshot.
	 * 
	 * @param snapshot
	 * 
     */
    private SnapshotInfo createSnapshotInfo( Gfa snapshot, Gfa clipped ) {

        /*
         *  Find the type of the snapshot
         *  X_SNAPSHOTS - SS intersects with the clipped polygon with an area > 3K.
         *  O_SNAPSHOTS - SS do not intersect with the clipped polygon
         */
	    Polygon ssp = GfaClip.getInstance().gfaToPolygonInGrid( snapshot );
	    Polygon clip = GfaClip.getInstance().gfaToPolygonInGrid( clipped );
	    
	    SnapshotType type = SnapshotType.O;
	    if ( ssp.intersects( clip ) ) {
	    	double area = PgenUtil.getSphPolyAreaInGrid( ssp.intersection( clip ) );
	    	if ( area > GfaRules.AREA_LIMIT ) {
	    		type = SnapshotType.X;
	    	}
	    }
	    
	    /*
	     *  Retrieve each snapshot's forecast time. 
	     *
	     *  Round UP   - X_SNAPSHOTS for development wording or
	     *               O_SNAPSHOTS for ending wording 
	     *  Round Down - X_SNAPSHOTS for ending wording or
	     *               O_SNAPSHOTS for development wording 
	     */
		int[] ftime = Gfa.getHourMinInt( snapshot.getForecastHours() );		
		double hmD = ftime[0] + ftime[1] / 60.0;
        int dvlpg_hr, endg_hr;
        
		if( type == SnapshotType.X ) {
			dvlpg_hr = (int)Math.ceil(hmD);
			endg_hr = (int)Math.floor(hmD);
		} else {
			dvlpg_hr = (int)Math.floor(hmD);
			endg_hr = (int)Math.ceil(hmD);
		}	    	    
	    		
		return new SnapshotInfo( type, ftime[0], ftime[1], dvlpg_hr, endg_hr );
		
	}

    /* 
	 * Creates a new SnapshotGrp.
	 * 
	 * @param snapshot
	 * 
     */
    private SnapshotGroup createSnapshotGroup( SnapshotInfo ssinfo, int start ) {
		
    	ArrayList<SnapshotInfo> sin = new ArrayList<SnapshotInfo>();
    	sin.add( ssinfo );
		
    	return new SnapshotGroup( ssinfo.type, ssinfo.hour, ssinfo.minute,
		                          ssinfo.dvlpg_hr, ssinfo.endg_hr, start, sin );
 		
	}
    
    /*
     * Gets the worst issue type from a set of snapshots.
     * 
     * Modified from ctbgfa.c => ctb_gfaWorstIssue().
     * 
     * @param ssList
     */
    private static String getGfaWorstIssueType ( ArrayList<Gfa> ssList ) {
    	    	                                                                            
        String worstIssue = "";       
               
        if ( ssList != null && ssList.size() > 0 ) {
            ArrayList<String> ssIssueTypes = new ArrayList<String>();
		    for ( Gfa ss : ssList ) {
		        ssIssueTypes.add( ss.getGfaIssueType() ) ;
		    }
		    
		    worstIssue = new String( gfaWorstIssueType( ssIssueTypes ) );
        }
 
		return worstIssue;
        
    }
    
    /*
     * Gets the worst issue type from a set of issue types.
     * 
     * Modified from ctbgfa.c => ctb_gfaWorstIssue().
     * 
     * @param issueTypes
     */
    private static String gfaWorstIssueType ( ArrayList<String> issueTypes ) {
    	    	                                                                            
        String worstIssue = "";       
        int nEl = issueTypes.size();
        
        // Return "" if no issueTypes.
        if ( nEl <= 0 ) {
        	return worstIssue;
        }
        
        // Default is "NRML".
        worstIssue = new String( "NRML" );  

        //If any of the input GFA is COR, return COR.
        for ( int ii = 0; ii < nEl; ii++ ) {
        	if ( issueTypes.get( ii ).equals( "COR" ) ) {
        		worstIssue = new String( "COR" );
        		return worstIssue;
        	}
        }

        //If any of the input GFA is AMD, return AMD.
        for ( int ii = 0; ii < nEl; ii++ ) {
        	if (  issueTypes.get( ii ).equals( "AMD" )) {
        		worstIssue = new String( "AMD" );
        		return worstIssue;
        	}
        }

        /*
         *  If all input GFAs are NEW, return NEW.
         *  If some of them are NEW, return AMD.
         */
        boolean allNew  = true;
        boolean someNew = false;

        for ( int ii = 0; ii < nEl; ii++ ) {                                                                                  
        	boolean thisIsNew = issueTypes.get( ii ).equals( "NEW" );
        	allNew = allNew && thisIsNew;
        	if ( thisIsNew ) someNew = true;
        }

        someNew = someNew && !allNew;

        if ( allNew ) {                                                                                 
        	worstIssue = new String( "NEW"  );
        	return worstIssue;                                                                               
        }
        else if ( someNew )  {                                                                               
        	worstIssue = new String( "AMD" );
        	return worstIssue;                                                                                  
        }

        /*
         *  If all input GFAs are CAN, return CAN.
         *  If some of them are CAN, return AMD.
         */
        boolean allCan  = true;
        boolean someCan = false;

        for ( int ii = 0; ii < nEl; ii++ ) {
        	boolean thisIsCan = issueTypes.get( ii ).equals( "CAN" );
        	allCan = allCan && thisIsCan;
        	if ( thisIsCan ) someCan = true;                                                                                  
        }

        someCan = someCan && !allCan;

        if ( allCan ) {                                                                            
        	worstIssue = new String( "CAN" );
        	return worstIssue;

        }
        else if ( someCan )  {                                                                                    
        	worstIssue = new String( "AMD" );              
        	return worstIssue;

        }
        
        // Finally, return "NRML"      
        return worstIssue;
        
    }
    
    
    /*
     * Gets the worst frequency from a set of snapshots.
     * 
     * Modified from ctbgfa.c => gfaCmpSeverity().
     * 
     * @param ssList
     */
	@SuppressWarnings("unchecked")
    private static String getGfaWorstFrequency ( ArrayList<Gfa> ssList ) {
    	    	                                                                            
        String worstFreq = "";       
               
        if ( ssList != null && ssList.size() > 0 ) {  
        	
        	String hazard = ssList.get(0).getGfaHazard();
        	
        	String freq = ssList.get(0).getGfaValue( Gfa.FREQUENCY );
        	
            if ( freq != null && freq.length() > 0 ) {    
            	
        		String xPath = GfaInfo.HAZARD_XPATH + "[@name='" + hazard + "']/*";
        		List<Node> nodes = GfaInfo.selectNodes( xPath );
                
        		ArrayList<String>  frequencyInfo = new ArrayList<String>();
        		for ( Node node : nodes ) {
         			if ( "dropdown".equalsIgnoreCase( node.getName() ) ){
         				String lblStr = node.valueOf( "@label" );
         				if ( lblStr != null &&  lblStr.equalsIgnoreCase( Gfa.FREQUENCY ) ) {
         				    List<Node> list = node.selectNodes( "value" );
         	                for( Node n : list ){
         	                	frequencyInfo.add( n.getText() );
         				    }
         				}
        			}
        		}
        		           	
            	worstFreq = new String( freq );
            	
            	for ( int ii = 1; ii < ssList.size(); ii++ ) {
            		worstFreq =  gfaWorstFrequency ( frequencyInfo, worstFreq, 
            				          ssList.get( ii ).getGfaValue( Gfa.FREQUENCY ) ) ;
            	}
            }
        }
 
		return worstFreq;
        
    }
 
    /*
     * Compares the severity of two GFA hazard/descriptor combinations and 
     * returns the most severe.  This relative severity ranking is simply 
     * based on the order of occurrence within the gfa.tbl so if they are not 
     * in descending order of severity, this function will not return correct 
     * results.
     * 	
     * Note that in the case of an error, value1 will be returned as the
     * highest value.  Also note that a value of "No Qualifier" will be  
     * treated as the lowest severity.  				
     * 
     * Modified from ctbgfa.c => gfaCmpSeverity().
     * 
     * @param ssList
     */
    private static String gfaWorstFrequency ( ArrayList<String> freqs,
    		                                  String value1, String value2 ) {
    	    	                                                                                    
        //Default.  If anything goes wrong 
        String worstFreq = new String( value1 );
      
        /*
         *  Check if either value is "No Qualifier".  This will appear in
         *  the choice list for the descriptor, but is to be treated as the 
         *  lowest priority.  Trick is -- it usually appears at the end of the 
         *  list which would give it the highest value if we didn't specifically 
         *  screen it out here.
         */
         if( value2.equalsIgnoreCase( "No Qualifier") ) {
             return worstFreq;
         }
            
         if ( value1.equalsIgnoreCase( "No Qualifier") ) {
        	 worstFreq = new String( value2 );
             return worstFreq;
         }                                 
         
         if ( freqs != null && freqs.size() > 0 ) {
        	 boolean findValue1, findValue2;
        	 for ( String fq : freqs ) {
        		 findValue1 = value1.equalsIgnoreCase( fq );
        		 findValue2 = value2.equalsIgnoreCase( fq );
        		 
        		 if ( findValue1 || findValue2 ) {
        			 if ( findValue1 ) {
        				 worstFreq = new String( value2 );
        			 }
        			 break;
        		 }
        	 }
         }
         
		 return worstFreq;
        
    }

}
