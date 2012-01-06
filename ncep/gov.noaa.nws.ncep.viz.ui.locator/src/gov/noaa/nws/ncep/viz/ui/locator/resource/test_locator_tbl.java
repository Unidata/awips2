package gov.noaa.nws.ncep.viz.ui.locator.resource;


import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;

import java.util.List;

import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.VizApp;


public class test_locator_tbl {
    
    public static void main(String args[]) throws Exception{
    	
        LocatorTableReader myloc = new LocatorTableReader("res/locator_tbl.xml");
    	//LocatorTableReader myloc = new LocatorTableReader(NmapCommon.getLocatorTable());     
        List<Locator> list = myloc.getLocatorTable();
        for(Locator itm : list){
            System.out.println("LoactorName = " + itm.getLocatorName() + 
            	", ShapefileName= "+ itm.getShapefileName()+ 	
                ", path = " + itm.getShapefilePath() + ", " +
                ", AttributeName = " + itm.getAttributeName() +
                ", AttributeID ="+ itm.getAttributeID()+
                ", UsedInSeek="+itm.isUsedInSeek()+
                ", StateID="+itm.getStateID()+
                ", latlonUnit="+itm.getDisplayOptions().getLatLonUnit()+
                ", distanceUnit="+itm.getDisplayOptions().getDistanceUnit()+
                ", rounding="+itm.getDisplayOptions().getRoundingToNearest()+
                ", Direction="+itm.getDisplayOptions().getDirectionUnit()+                
                ", DisplayAttribte ="+itm.getDisplayOptions().getDisplayAttribute()+
                ", IsMultipleDefault ="+itm.getDisplayOptions().isIsMultipleDefault()
            );
       }
    } 
}        
