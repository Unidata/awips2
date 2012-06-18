package gov.noaa.nws.ncep.standalone.testConverter;

import gov.noaa.nws.ncep.common.staticdata.SPCCounty;
import gov.noaa.nws.ncep.standalone.util.ProductConverter;
import gov.noaa.nws.ncep.standalone.util.Util;
import gov.noaa.nws.ncep.standalone.xmlConverter.XmlUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;
import gov.noaa.nws.ncep.ui.pgen.elements.AvnText;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.elements.MidCloudText;
import gov.noaa.nws.ncep.ui.pgen.elements.Track;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet;
import gov.noaa.nws.ncep.ui.pgen.sigmet.Volcano;
import gov.noaa.nws.ncep.ui.pgen.tca.*;
//import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.TimeZone;


/**
 * TestXmlConvert
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/18/2010    271         Q. Zhou     Initial created
 * 6/17/2010    271         Q. Zhou     Add Gfa 
 * 9/09/2010    271         Q. Zhou     Added Main for cml. Removed Localizaton.  Use environment vars.
 * 12/2/2010    271         Q. Zhou     Added Cloud/Turbulence. Added symLabel.
 * 										Added Volcano/ash and handle empty layers.
 * 										Modified Jet's text comparison. 
 * 1/06/2011   137         Q. Zhou      Created local copy of ProductConverter.
 * 1/20/2011   137         Q. Zhou      Modified commonColors for read colors on a list								   
 * 2/08/2011   137         Q. Zhou      Modified output message.
 * 										Added common label comparison			
 * 11/1/2011   137         qzhou        Modified due to pgen refactor. Removed pgen.original							
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
public class TestXmlConvert  {
	/**
	 * @param args
	 */
	public boolean testXml(String original, String converted) throws IOException {
	    	
		if ( !original.endsWith(".xml")) {
			System.out.println("The first xml file does not exist");
			return false;
		}
		if ( !converted.endsWith(".xml")) {
			System.out.println("The second xml file does not exist");
			return false;
		}
		
	    //get products
	    Products products1  = new Products();
	    products1 = Util.read( original);
	    if (products1 == null) {
	    	System.out.println("The first file can't be read. Check the format.");
	    	return false;
	    }
	    List<Product> prod1 = ProductConverter.convert( products1 );
	    
	    Products products2  = new Products();
	    products2 = Util.read( converted);
	    if (products2 == null) {
	    	System.out.println("The second file can't be read. Check the format.");
	    	return false;
	    }
	    List<Product> prod2 = ProductConverter.convert( products2 );
	    
	    
	    if (prod1.isEmpty() || prod2.isEmpty()) {
	    	System.out.println("The compared files contain empty record.");
	    	return false;
	    }    
	    if (prod1.size() != prod2.size()) {
	    	System.out.println("*** "+ original +" and "+ converted +" contain different record sizes.\n");
	    	return false;
	    }
	    
	    //loop for layers
	    for (int i=0; i< prod1.size(); i++) {
	    	List<Layer> lay1 = prod1.get(i).getLayers();
	    	List<Layer> lay2 = prod2.get(i).getLayers();
	    	
	    	if (lay1.isEmpty() || lay2.isEmpty()) {
		    	System.out.println("The compared files contain empty record.");
		    	return false;
		    }
	    	if (lay1.size() != lay2.size()) {
	    		System.out.println("*** The two files "+ original +" and "+ converted +" contain the different record sizes.\n");
		    	return false;
		    }
	    	
	    	//loop for components
	    	for (int j=0; j<lay1.size() ; j++) {	    		
	    		List<AbstractDrawableComponent> adc1 = lay1.get(j).getDrawables();
	    		List<AbstractDrawableComponent> adc2 = lay2.get(j).getDrawables();
	    		
	    		//handle volcano & ash empty layer
	    		if ( adc1.isEmpty() && adc2.isEmpty()) {
	    		}
	    		else {
    		
	    		if (adc1.isEmpty() || adc2.isEmpty()) {
			    	System.out.println("The compared files contain empty record.");
			    	return false;
			    }
	    		if (adc1.size() != adc2.size()) {
	    			System.out.println("*** The two files "+ original +" and "+ converted +" contain the different record sizes.\n");
			    	return false;
			    }
	    		
	    		List<DrawableElement> de1list = new ArrayList<DrawableElement>();
	    	    List<DrawableElement> de2list = new ArrayList<DrawableElement>();
	    	    List<AbstractDrawableComponent> cl1list = new ArrayList<AbstractDrawableComponent>();
	    	    List<AbstractDrawableComponent> cl2list = new ArrayList<AbstractDrawableComponent>();
	    	    
	    		for (int k=0; k<adc1.size(); k++) {
	    			
	    			if (adc1.get(k) instanceof DrawableElement) {
	    				de1list.add ( adc1.get(k).getPrimaryDE());	    				
	    			}
	    			else if (adc1.get(k) instanceof DECollection) {		    				
	    				cl1list.add ( adc1.get(k));	    				
	    			}
	    		}
	    		for (int k=0; k<adc2.size(); k++) {
	    			
	    			if (adc2.get(k) instanceof DrawableElement) {
	    				de2list.add ( adc2.get(k).getPrimaryDE());	    				
	    			}
	    			else if (adc2.get(k) instanceof DECollection) {		    				
	    				cl2list.add ( adc2.get(k));	    				
	    			}
	    		}
	    
	    		//compare collections
	    		int p=0, q=0;
	    		if (cl1list.size() != cl2list.size()) {
	    			System.out.println("*** The two files "+ original +" and "+ converted +" contain the different record sizes.\n");
			    	return false;
			    }
	    		
	    		String pgenName1 ="", pgenName2 ="";
	    		for ( p=0; p< cl1list.size(); p++) {
	    			int cnt = 0;
	    			for ( q=0; q< cl2list.size(); q++) {
	    			//if (adc1.get(k) instanceof DECollection) {	    			
	    			//if (cl1list.get(p) instanceof DrawableElement) {
	    				pgenName1 = cl1list.get(p).getName();
	    				pgenName2 = cl2list.get(q).getName();
	    				
	    				if (pgenName1.equalsIgnoreCase(pgenName2)) {
	    					if (pgenName1.equalsIgnoreCase("Watch")) {
	    						if (compareWatch(cl1list.get(p), cl2list.get(q), p)) {
	    							cl2list.remove(q);
	    							cnt++;
	    							break;
	    						}
	    					}
	    					else if (pgenName1.equalsIgnoreCase("Jet")) {
	    						if (compareJet(cl1list.get(p), cl2list.get(q), p)) {
	    							cl2list.remove(q);
	    							cnt++;
	    							break;
	    						}
	    					}
	    					else if (pgenName1.equalsIgnoreCase("Contours")) {
	    						if (compareContour(cl1list.get(p), cl2list.get(q), p)) {
	    							cl2list.remove(q);
	    							cnt++;
	    							break;
	    						}
	    					}
	    					else if (pgenName1.equalsIgnoreCase("Outlook")) {
	    						if (compareOutlook(cl1list.get(p), cl2list.get(q), p)) {
	    							cl2list.remove(q);
	    							cnt++;
	    							break;
	    						}
	    					}
	    					else if (pgenName1.equalsIgnoreCase("Cloud")) {
	    						if (compareCloud(cl1list.get(p), cl2list.get(q), p)) {
	    							cl2list.remove(q);
	    							cnt++;
	    							break;
	    						}
	    					}
	    					else if (pgenName1.equalsIgnoreCase("Turbulence")) {
	    						if (compareTurbulence(cl1list.get(p), cl2list.get(q), p)) {
	    							cl2list.remove(q);
	    							cnt++;
	    							break;
	    						}
	    					}
	    					else if (pgenName1.equalsIgnoreCase("labeledSymbol")) {
	    						if (compareSymLabel(cl1list.get(p), cl2list.get(q), p)) {
	    							cl2list.remove(q);
	    							cnt++;
	    							break;
	    						}
	    					}
	    					else if (pgenName1.equalsIgnoreCase("labeledCommon")) {
	    						if (comparecommonLabel(cl1list.get(p), cl2list.get(q), p)) {
	    							cl2list.remove(q);
	    							cnt++;
	    							break;
	    						}
	    					}
	    				}
	    			}
	    			if (cnt== 0) {
						System.out.println("\nThe DECollection #" + (p+1) + " " + pgenName1  + " in the first file is not contained in the second file.");
						System.out.println("*** The two files "+ original +" and "+ converted +" contain the different information.\n");
						return false;
					}	    			
	    		}
	    		
	    		//compare DEs
	    		int m=0, n=0;
	    		if (de1list.size() != de2list.size()) {
	    			System.out.println(original +" and "+ converted +" contain different record sizes.\n");
			    	return false;
			    }
	    		
	    		for ( m=0; m< de1list.size(); m++) {
	    			int cnt = 0;
	    			for ( n=0; n< de2list.size(); n++) {
	    				pgenName1 = de1list.get(m).getPgenCategory(); //getPgenCategory=Lines, getName=FILLED_ARROW
	    				pgenName2 = de2list.get(n).getPgenCategory();
	    				
	    				if (pgenName1.equalsIgnoreCase(pgenName2)) {
	    					if (pgenName1.equalsIgnoreCase("Lines")) {
	    						if (compareLines(de1list.get(m), de2list.get(n), m, "DE", "")) {
	    							de2list.remove(n);
	    							cnt++;
	    							break;
	    						}
	    					}
	    					else if (pgenName1.equalsIgnoreCase("Front")) {
	    						if (compareFront(de1list.get(m), de2list.get(n), m, "DE")) {
	    							de2list.remove(n);
	    							cnt++;
	    							break;
	    						}
	    					}
	    						
	    					else if (pgenName1.equalsIgnoreCase("Symbol") || pgenName1.equalsIgnoreCase("Combo")
	    							|| pgenName1.equalsIgnoreCase("Marker")) {
	    						if (compareSymbol(de1list.get(m), de2list.get(n), m, "DE", "")) {
	    							de2list.remove(n);
	    							cnt++;
	    							break;
	    						}
	    					}
	    						
	    					else if (pgenName1.equalsIgnoreCase("Vector")) {
	    						if (compareVector(de1list.get(m), de2list.get(n), m, "DE", "")) {
	    							de2list.remove(n);
	    							cnt++;
	    							break;
	    						}
	    					}
	    						
	    					else if (pgenName1.equalsIgnoreCase("Track")) {
	    						if (compareTrack(de1list.get(m), de2list.get(n), m, "DE")) {
	    							de2list.remove(n);
	    							cnt++;
	    							break;
	    						}
	    					}
	    						
	    					else if (pgenName1.equalsIgnoreCase("Sigmet") &&
	    							de1list.get(m).getPgenType().equalsIgnoreCase("VOLC_SIGMET")) {
	    						if (compareVolc(de1list.get(m), de2list.get(n), m, "DE")) {
	    							de2list.remove(n);
	    							cnt++;
	    							break;
	    						}
	    					}
	    					
	    					else if (pgenName1.equalsIgnoreCase("Sigmet") &&
	    							de1list.get(m).getPgenType().equalsIgnoreCase("VACL_SIGMET")) {
	    						if (compareVolcAsh(de1list.get(m), de2list.get(n), m, "DE")) {
	    							de2list.remove(n);
	    							cnt++;
	    							break;
	    						}
	    					}
	    					
	    					else if (pgenName1.equalsIgnoreCase("Sigmet")) {
	    						if (compareSigmet(de1list.get(m), de2list.get(n), m, "DE")) {
	    							de2list.remove(n);
	    							cnt++;
	    							break;
	    						}
	    					}
	    						
	    					else if (pgenName1.equalsIgnoreCase("Met") 
	    							&& de1list.get(m).getPgenType().equalsIgnoreCase("TCA")) {
	    						if (compareTca(de1list.get(m), de2list.get(n), m, "DE")) {
	    							de2list.remove(n);
	    							cnt++;
	    							break;
	    						}
	    					}
	    					
	    					else if (pgenName1.equalsIgnoreCase("Met") 
	    							&& de1list.get(m).getPgenType().equalsIgnoreCase("GFA")) {
	    						if (compareGfa(de1list.get(m), de2list.get(n), m, "DE")) {
	    							de2list.remove(n);
	    							cnt++;
	    							break;
	    						}
	    					}
	    					
	    					else if (pgenName1.equalsIgnoreCase("Text")) {
	    						if (compareText(de1list.get(m), de2list.get(n), m, "DE", "")) {
	    							de2list.remove(n);
	    							cnt++;
	    							break;
	    						}
	    					}
	    						
	    					else if (pgenName1.equalsIgnoreCase("Arc")) {
	    						if (compareArc(de1list.get(m), de2list.get(n), m, "DE")) {
	    							de2list.remove(n);
	    							cnt++;
	    							break;
	    						}
	    					}
	    						
	    				}	    				
	    			}
	    			
	    			if (cnt== 0) {
						System.out.println("\nThe DE #" + (m+1) + " " + pgenName1  + " in the first file is not contained in the second file.");
						System.out.println("*** The two files "+ original +" and "+ converted +" contain the different information.\n");
						
						return false;
					}
	    		}
	    	} //end of handel volc & ash	
	    	}
	    }	
	    System.out.println("  Note for watch, the WFOs, WathcAreaNm, HalfWidthSm, HalfWidthNm, and EndPointVor fields are ignored.\n" +
	    		"  Note for line, the SizeScale field is ignored.  The FillPattern field is ignored.\n" +
	    		"  Note for text, the MaskText and OutlineText fields are ignored.\n" +
	    		"  Note for sigmet, the EditableAttrFromLine, editableAttrFreeText, sizeScale and FillPattern fields are ignored.\n" +
	    		"  Note for tca, the Breakpoints fields are ignored." +
	    		"  Note for ark, the AxisRatio, StartAngle, EndAngle fields are ignored.\n");
	    System.out.println("The two files " + original +" and "+ converted +" contain the same information.\n");
	    
	    return true;
	}
	
	
	private static Boolean compareWatch(AbstractDrawableComponent cl1, AbstractDrawableComponent cl2, int m) {
		DrawableElement de1 = null, de2 = null;
		Iterator<DrawableElement> iter1 = cl1.createDEIterator();
		Iterator<DrawableElement> iter2 = cl2.createDEIterator();
		List<DrawableElement> l1 = new ArrayList<DrawableElement>();
		List<DrawableElement> l2 = new ArrayList<DrawableElement>();
		
		while (iter1.hasNext())
			l1.add(iter1.next());
		while (iter2.hasNext())
			l2.add(iter2.next());
		if (l1.size() != l2.size())
			return false;
		
		int cnt = 0; int a=0, b=0;
		for ( a=0; a< l1.size(); a++) {
			cnt = 0;
			for ( b=0; b< l2.size(); b++) {
				de1 = l1.get(a);
				de2 = l2.get(b);
				if (de1.getPgenCategory().equalsIgnoreCase("Lines")) {
					if (compareLines(de1, de2, m, "DECollection", "WATCH's")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
				else if (de1.getPgenCategory().equalsIgnoreCase("watch")) {
					if (compareWatchDE(de1, de2, m, "DECollection", "WATCH's")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
			}
			
			if (cnt== 0) {
				System.out.println("The DECollection #" +(m+1) + " WATCH's " + de1.getPgenCategory() + " in the first file is not contained in the second file.");
				return false;
			}

		}		
		return true;
	}

	private static Boolean compareJet(AbstractDrawableComponent cl1, AbstractDrawableComponent cl2, int m) {
		DrawableElement de1 = null, de2 = null;
		Iterator<DrawableElement> iter1 = cl1.createDEIterator();
		Iterator<DrawableElement> iter2 = cl2.createDEIterator();
		List<DrawableElement> l1 = new ArrayList<DrawableElement>();
		List<DrawableElement> l2 = new ArrayList<DrawableElement>();
		
		while (iter1.hasNext())
			l1.add(iter1.next());
		while (iter2.hasNext())
			l2.add(iter2.next());
		if (l1.size() != l2.size())
			return false;
		
		int cnt = 0; int a=0; int b=0;
		for ( a=0; a< l1.size(); a++) {
			cnt = 0;
			for ( b=0; b< l2.size(); b++) {
				de1 = l1.get(a);
				de2 = l2.get(b);
				if (de1.getPgenCategory().equalsIgnoreCase("text")) {
					if (compareText(de1, de2, m, "DECollection", "JET's")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
				else if (de1.getPgenCategory().equalsIgnoreCase("vector")) {
					if (compareVector(de1, de2, m,"DECollection", "JET's")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
				else if (de1.getPgenCategory().equalsIgnoreCase("Lines")) {
					if (compareLines(de1, de2, m, "DECollection", "JET's")) {  //"collection" used to output text
						l2.remove(b);
						cnt++;
						break;
					}
				}
			}
			
			if (cnt== 0) {
				System.out.println("The DECollection #" + (m+1) + " JET's " + de1.getPgenCategory() + " in the first file is not contained in the second file.");
				return false;
			}
		}
				
		return true;
	}

	private static Boolean compareContour(AbstractDrawableComponent cl1, AbstractDrawableComponent cl2, int m) {
		DrawableElement de1 = null, de2 = null;
		Iterator<DrawableElement> iter1 = cl1.createDEIterator();
		Iterator<DrawableElement> iter2 = cl2.createDEIterator();
		List<DrawableElement> l1 = new ArrayList<DrawableElement>();
		List<DrawableElement> l2 = new ArrayList<DrawableElement>();
		
		while (iter1.hasNext())
			l1.add(iter1.next());
		while (iter2.hasNext())
			l2.add(iter2.next());
		if (l1.size() != l2.size())
			return false;
		
		int cnt = 0; int a=0, b=0;
		for ( a=0; a< l1.size(); a++) {
			cnt = 0;
			for ( b=0; b< l2.size(); b++) {
				de1 = l1.get(a);
				de2 = l2.get(b);
				if (de1.getPgenCategory().equalsIgnoreCase("text")) {
					if (compareText(de1, de2, m, "DECollection", "CONTOURS'")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
				else if (de1.getPgenCategory().equalsIgnoreCase("Lines")) {
					if (compareLines(de1, de2, m, "DECollection", "CONTOURS'")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
				else if (de1.getPgenCategory().equalsIgnoreCase("Symbol")) {
					if (compareSymbol(de1, de2, m, "DECollection", "CONTOURS'")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
			}
			
			if (cnt== 0) {
				System.out.println("The DECollection #" + (m+1) + " CONTOURS' " + de1.getPgenCategory() + " in the first file is not contained in the second file.");
				return false;
			}
		}

		return true;
	}

	private static Boolean compareOutlook(AbstractDrawableComponent cl1, AbstractDrawableComponent cl2, int m) {
		//compare outlooktype first: outlook hailotlk, ...
		if (!cl1.getPgenType().equalsIgnoreCase( cl2.getPgenType())) {
			System.out.println("The DECollection #" + (m+1) + " " +cl1.getPgenType() + " in the first file is not contained in the second file.");			
			return false;
		}
		
		DrawableElement de1 = null, de2 = null;
		Iterator<DrawableElement> iter1 = cl1.createDEIterator();
		Iterator<DrawableElement> iter2 = cl2.createDEIterator();
		List<DrawableElement> l1 = new ArrayList<DrawableElement>();
		List<DrawableElement> l2 = new ArrayList<DrawableElement>();
		
		while (iter1.hasNext())
			l1.add(iter1.next());
		while (iter2.hasNext())
			l2.add(iter2.next());
		if (l1.size() != l2.size())
			return false;
		
		int cnt = 0; int a=0, b=0;
		for ( a=0; a< l1.size(); a++) {
			
			cnt = 0;
			for ( b=0; b< l2.size(); b++) {
				de1 = l1.get(a);
				de2 = l2.get(b);
				if (de1.getPgenCategory().equalsIgnoreCase("text")) {
					if (compareText(de1, de2, m, "DECollection", cl1.getPgenType()+"'s")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
				else if (de1.getPgenCategory().equalsIgnoreCase("Lines")) {
					if (compareLines(de1, de2, m, "DECollection", cl1.getPgenType()+"'s")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
			}
			
			if (cnt== 0) {
				System.out.println("The DECollection #" + (m+1) + " " +cl1.getPgenType() +"'s " + de1.getPgenCategory() + " in the first file is not contained in the second file.");
				return false;
			}
		}

		return true;
	}
	
	private static Boolean compareCloud(AbstractDrawableComponent cl1, AbstractDrawableComponent cl2, int m) {
		if (!cl1.getPgenType().equalsIgnoreCase( cl2.getPgenType())) {
			System.out.println("The DECollection #" + (m+1) + " " +cl1.getPgenType() + " in the first file is not contained in the second file.");			
			return false;
		}
		
		DrawableElement de1 = null, de2 = null;
		Iterator<DrawableElement> iter1 = cl1.createDEIterator();
		Iterator<DrawableElement> iter2 = cl2.createDEIterator();
		List<DrawableElement> l1 = new ArrayList<DrawableElement>();
		List<DrawableElement> l2 = new ArrayList<DrawableElement>();
		
		while (iter1.hasNext())
			l1.add(iter1.next());
		while (iter2.hasNext())
			l2.add(iter2.next());
		if (l1.size() != l2.size())
			return false;
		
		int cnt = 0; int a=0, b=0;
		for ( a=0; a< l1.size(); a++) {
			
			cnt = 0;
			for ( b=0; b< l2.size(); b++) {
				de1 = l1.get(a);
				de2 = l2.get(b);
				if (de1.getPgenCategory().equalsIgnoreCase("text")) {
					if (compareCloudText(de1, de2, m, "DECollection", cl1.getPgenType()+"'s")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
				else if (de1.getPgenCategory().equalsIgnoreCase("Lines")) {
					if (compareLines(de1, de2, m, "DECollection", cl1.getPgenType()+"'s")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
			}
			
			if (cnt== 0) {
				System.out.println("The DECollection #" + (m+1) + " " +cl1.getPgenType() +"'s " + de1.getPgenCategory() + " in the first file is not contained in the second file.");
				return false;
			}
		}
		
		return true;
	}
	
	private static Boolean compareTurbulence(AbstractDrawableComponent cl1, AbstractDrawableComponent cl2, int m) {
		if (!cl1.getPgenType().equalsIgnoreCase( cl2.getPgenType())) {
			System.out.println("The DECollection #" + (m+1) + " " +cl1.getPgenType() + " in the first file is not contained in the second file.");			
			return false;
		}
		
		DrawableElement de1 = null, de2 = null;
		Iterator<DrawableElement> iter1 = cl1.createDEIterator();
		Iterator<DrawableElement> iter2 = cl2.createDEIterator();
		List<DrawableElement> l1 = new ArrayList<DrawableElement>();
		List<DrawableElement> l2 = new ArrayList<DrawableElement>();
		
		while (iter1.hasNext())
			l1.add(iter1.next());
		while (iter2.hasNext())
			l2.add(iter2.next());
		if (l1.size() != l2.size())
			return false;
		
		int cnt = 0; int a=0, b=0;
		for ( a=0; a< l1.size(); a++) {
			
			cnt = 0;
			for ( b=0; b< l2.size(); b++) {
				de1 = l1.get(a);
				de2 = l2.get(b);
				if (de1.getPgenCategory().equalsIgnoreCase("text")) {
					if (compareText(de1, de2, m, "DECollection", cl1.getPgenType()+"'s")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
				else if (de1.getPgenCategory().equalsIgnoreCase("Lines")) {
					if (compareLines(de1, de2, m, "DECollection", cl1.getPgenType()+"'s")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
			}
			
			if (cnt== 0) {
				System.out.println("The DECollection #" + (m+1) + " " +cl1.getPgenType() +"'s " + de1.getPgenCategory() + " in the first file is not contained in the second file.");
				return false;
			}
		}
		
		return true;
	}
	
	private static Boolean compareSymLabel(AbstractDrawableComponent cl1, AbstractDrawableComponent cl2, int m) {
		if (!cl1.getPgenType().equalsIgnoreCase( cl2.getPgenType())) {
			System.out.println("The DECollection #" + (m+1) + " " +cl1.getPgenType() + " in the first file is not contained in the second file.");			
			return false;
		}
		
		DrawableElement de1 = null, de2 = null;
		Iterator<DrawableElement> iter1 = cl1.createDEIterator();
		Iterator<DrawableElement> iter2 = cl2.createDEIterator();
		List<DrawableElement> l1 = new ArrayList<DrawableElement>();
		List<DrawableElement> l2 = new ArrayList<DrawableElement>();
		
		while (iter1.hasNext())
			l1.add(iter1.next());
		while (iter2.hasNext())
			l2.add(iter2.next());
		if (l1.size() != l2.size())
			return false;
		
		int cnt = 0; int a=0, b=0;
		for ( a=0; a< l1.size(); a++) {
			
			cnt = 0;
			for ( b=0; b< l2.size(); b++) {
				de1 = l1.get(a);
				de2 = l2.get(b);
				if (de1.getPgenCategory().equalsIgnoreCase("text")) {
					if (compareText(de1, de2, m, "DECollection", cl1.getPgenType()+"'s")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
				else if (de1.getPgenCategory().equalsIgnoreCase("Symbol")) {
					if (compareSymbol(de1, de2, m, "DECollection", cl1.getPgenType()+"'s")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
			}
			
			if (cnt== 0) {
				System.out.println("The DECollection #" + (m+1) + " " +cl1.getPgenType() +"'s " + de1.getPgenCategory() + " in the first file is not contained in the second file.");
				return false;
			}
		}
		
		return true;
	}
	
	private static Boolean comparecommonLabel(AbstractDrawableComponent cl1, AbstractDrawableComponent cl2, int m) {
		if (!cl1.getPgenType().equalsIgnoreCase( cl2.getPgenType())) {
			System.out.println("The DECollection #" + (m+1) + " " +cl1.getPgenType() + " in the first file is not contained in the second file.");			
			return false;
		}
		
		DrawableElement de1 = null, de2 = null;
		Iterator<DrawableElement> iter1 = cl1.createDEIterator();
		Iterator<DrawableElement> iter2 = cl2.createDEIterator();
		List<DrawableElement> l1 = new ArrayList<DrawableElement>();
		List<DrawableElement> l2 = new ArrayList<DrawableElement>();
		
		while (iter1.hasNext())
			l1.add(iter1.next());
		while (iter2.hasNext())
			l2.add(iter2.next());
		if (l1.size() != l2.size())
			return false;
		
		int cnt = 0; int a=0, b=0;
		for ( a=0; a< l1.size(); a++) {
			
			cnt = 0;
			for ( b=0; b< l2.size(); b++) {
				de1 = l1.get(a);
				de2 = l2.get(b);
				if (de1.getPgenCategory().equalsIgnoreCase("text")) {
					if (compareText(de1, de2, m, "DECollection", cl1.getPgenType()+"'s")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
				else if (de1.getPgenCategory().equalsIgnoreCase("Symbol")) {
					if (compareSymbol(de1, de2, m, "DECollection", cl1.getPgenType()+"'s")) {
						l2.remove(b);
						cnt++;
						break;
					}
				}
			}
			
			if (cnt== 0) {
				System.out.println("The DECollection #" + (m+1) + " " +cl1.getPgenType() +"'s " + de1.getPgenCategory() + " in the first file is not contained in the second file.");
				return false;
			}
		}
		
		return true;
	}
	
	
	private static Boolean commonPoints(DrawableElement de1, DrawableElement de2, int m) {
		List points1 = de1.getPoints();
		List points2 = de2.getPoints();
		if (points1.size() != points2.size()) {
			return false;
		}
		
		for (int i=0; i<points1.size(); i++) {
			String[] result1 = points1.get(i).toString().split(",");
			String[] result2 = points2.get(i).toString().split(",");  
			//tag2vgf made the precision change.
			if ( Math.abs(Float.valueOf(result1[0].substring(1)) - Float.valueOf(result2[0].substring(1)) ) >0.01 //0.000001 
					|| Math.abs(Float.valueOf(result1[1].trim()) - Float.valueOf(result2[1].trim()) ) >0.01) {
				
				return false;
			}
		}
		return true;
	}
	
	private static Boolean commonColors(DrawableElement de1, DrawableElement de2) { 		
		int col1 = 0;
		int col2 = 0;
				
		col1 = XmlUtil.getColorTag(XmlUtil.getColorMaj(de1));
		col2 = XmlUtil.getColorTag(XmlUtil.getColorMaj(de2));
		if (col1 != col2)
			return false;	
		
		else if (de1.getColors().length != 1 && de2.getColors().length != 1) {
			col1 = XmlUtil.getColorTag(XmlUtil.getColorMin(de1));
			col2 = XmlUtil.getColorTag(XmlUtil.getColorMin(de2));
		
			if (col1 != col2)
				return false;
		}
		 
		return true;
	}
	
	/*private static Boolean commonColors(String color1, String color2) { //java.awt.Color[r=0,g=255,b=255]		
		String col1="", col2="";
//		if ( !color1.equalsIgnoreCase(color2) ) {
//    		
//			col1 =color1.substring(color1.indexOf("[")+1, color1.indexOf("]"));
//			col2 =color2.substring(color2.indexOf("[")+1, color2.indexOf("]"));
//			
//			// XmlUtil.getColorTag(col1)[1] ==0 means the color is in the vgf color table
//			if (XmlUtil.getColorTag(col1)[1] ==0 && XmlUtil.getColorTag(col2)[1] ==0 )
//				return false;
//		}

//		if ( !color1.equalsIgnoreCase(color2) ) {
//			col1 = XmlUtil.getColorTag(XmlUtil.getColorMaj(de1));
//			col2 = XmlUtil.getColorTag(XmlUtil.getColorMaj(de2));
//			Float f1 = Float.valueOf(col1);  //.floatValue();
//			Float f2 = Float.valueOf(col2); //.floatValue();
//			
//			if (f1.intValue() == f2.intValue())   	
//				return true;
//			else
//				return false;			
//		}
		
		return true;
	}*/

	
	private static Boolean compareWatchDE(DrawableElement de1, DrawableElement de2, int m, String deCollection, String collection) {
		if (commonPoints(de1, de2, m) == false)
			return false;
		
		if (commonColors(de1, de2) == false)
			return false;
				
		//compare counties and states
		List<SPCCounty> counties1 = ((WatchBox)de1).getCountyList();
		List<SPCCounty> counties2 = ((WatchBox)de2).getCountyList();
		int i=0, j=0;
		if (counties1.size() != counties2.size()) {
			return false;
		}
		
		for ( i=0; i<counties1.size(); i++) {
			String result1 = counties1.get(i).getFips();
			int cnt = 0;
			for ( j=0; j<counties2.size(); j++) {
				String result2 = counties2.get(j).getFips();  //tag2vgf made the precision change.
				
				if ( result1.equalsIgnoreCase(result2)) { // fips decides county points and states
					counties2.remove(j);
					cnt++;
					break;
				}						
			}
			
			if (cnt== 0) { //no any match
				System.out.println("The DECollection #" + (m+1) + " WATCH's " +de1.getPgenCategory()+ " in the first file has a county that is not contained in the second file.");
				return false;
			}
			
		}
		
		SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy/HHmm"); //don't compare seconds
		sdf.setTimeZone( TimeZone.getTimeZone("GMT"));
		String t1="",t2="",t3="",t4="",t5="",t6="",t7="",t8="";
		
		Calendar c1 = ((WatchBox)de1).getStatusExpTime();
		if (c1 != null) 
			t1 = sdf.format( c1.getTime());				
		Calendar c2 = ((WatchBox)de2).getStatusExpTime();
		if (c2 != null) 
			t2 = sdf.format( c2.getTime());
		Calendar c3 = ((WatchBox)de1).getStatusValidTime();
		if (c3 != null) 
			t3 = sdf.format( c3.getTime());
		Calendar c4 = ((WatchBox)de2).getStatusValidTime();
		if (c4 != null) 
			t4 = sdf.format( c4.getTime());
		Calendar c5 = ((WatchBox)de1).getExpTime();
		if (c5 != null) 
			t5 = sdf.format( c5.getTime());
		Calendar c6 = ((WatchBox)de2).getExpTime();
		if (c6 != null) 
			t6 = sdf.format( c6.getTime());
		Calendar c7 = ((WatchBox)de1).getIssueTime();
		if (c7 != null) 
			t7 = sdf.format( c7.getTime());
		Calendar c8 = ((WatchBox)de2).getIssueTime();
		if (c8 != null) 
			t8 = sdf.format( c8.getTime());
		
		String s1 = ((WatchBox)de1).getStatusForecaster();
		String s2 = ((WatchBox)de2).getStatusForecaster();	
		String s3 = ((WatchBox)de1).getFromLine();
		String s4 = ((WatchBox)de2).getFromLine();
		String s5 = ((WatchBox)de1).getEndPointAnc().split("\\s+")[3];
		String s6 = ((WatchBox)de2).getEndPointAnc().split("\\s+")[3];
		String s7 = ((WatchBox)de1).getForecaster();
		String s8 = ((WatchBox)de2).getForecaster();
		String s9 = ((WatchBox)de1).getWatchType();
		String s10 = ((WatchBox)de2).getWatchType();
		String s11 = ((WatchBox)de1).getAdjAreas();
		String s12 = ((WatchBox)de2).getAdjAreas();
		String s13 = ((WatchBox)de1).getTimeZone();
		String s14 = ((WatchBox)de2).getTimeZone();
		String s15 = ((WatchBox)de1).getSeverity();
		String s16 = ((WatchBox)de2).getSeverity();
		String s17 = ((WatchBox)de1).getIssueStatus();
		String s18 = ((WatchBox)de2).getIssueStatus();
		
		if (  ((WatchBox)de1).getPgenType().equalsIgnoreCase(((WatchBox)de2).getPgenType())
		    && ( ((s1==null || s1.equalsIgnoreCase("")) && (s2==null || s2.equalsIgnoreCase("")) ) 
		    		|| (s1 !=null && s2 !=null && s1.equalsIgnoreCase(s2)) )
		    && t1.equalsIgnoreCase(t2) 
		    && t3.equalsIgnoreCase(t4) 
		    && ((WatchBox)de1).getDiscussion() ==  ((WatchBox)de2).getDiscussion()
		    && ( ((s3==null || s3.equalsIgnoreCase("")) && (s4==null || s4.equalsIgnoreCase("")) ) 
		    		|| (s3 !=null && s4 !=null && s3.equalsIgnoreCase(s4)) )
		    && ( ((s5==null || s5.equalsIgnoreCase("")) && (s6==null || s6.equalsIgnoreCase("")) ) 
		    		|| (s5 !=null && s6 !=null && s5.equalsIgnoreCase(s6)) )
		    && ( ((s7==null || s7.equalsIgnoreCase("")) && (s8==null || s8.equalsIgnoreCase("")) ) 
		    		|| (s7 !=null && s8 !=null && s7.equalsIgnoreCase(s8)) )
		    && ((WatchBox)de1).getContWatch() ==  ((WatchBox)de2).getContWatch()
		    && ((WatchBox)de1).getWatchNumber() == ((WatchBox)de2).getWatchNumber()
		    && ( ((s9==null || s9.equalsIgnoreCase("")) && (s10==null || s10.equalsIgnoreCase("")) ) 
		    		|| (s9 !=null && s10 !=null && s9.equalsIgnoreCase(s10)) )
		    && ((WatchBox)de1).getIssueFlag() ==  ((WatchBox)de2).getIssueFlag()
		    && ((WatchBox)de1).getReplWatch() == ((WatchBox)de2).getReplWatch()
		    && ( ((s11==null || s11.equalsIgnoreCase("")) && (s12==null || s12.equalsIgnoreCase("")) ) 
		    		|| (s11 !=null && s12 !=null && s11.equalsIgnoreCase(s12)) )
		    && ((WatchBox)de1).getMoveSpeed() == ((WatchBox)de2).getMoveSpeed()
		    && ((WatchBox)de1).getMoveDir() == ((WatchBox)de2).getMoveDir()	
		    && ((WatchBox)de1).getTop() == ((WatchBox)de2).getTop()
		    && ((WatchBox)de1).getGust() == ((WatchBox)de2).getGust()
		    && ((WatchBox)de1).getHailSize() == ((WatchBox)de2).getHailSize()
		    && ( ((s13==null || s13.equalsIgnoreCase("")) && (s14==null || s14.equalsIgnoreCase("")) ) 
		    		|| (s13 !=null && s14 !=null && s13.equalsIgnoreCase(s14)) )
		    /*&& ( ((s15==null || s15.equalsIgnoreCase("")) && (s16==null || s16.equalsIgnoreCase("")) ) 
		    		|| (s15 !=null && s16 !=null && s15.equalsIgnoreCase(s16)) )*/
		    && t5.equalsIgnoreCase(t6) 
		    && t7.equalsIgnoreCase(t8) 
		    && ( ((s17==null || s17.equalsIgnoreCase("")) && (s18==null || s18.equalsIgnoreCase("")) ) 
		    		|| (s17 !=null && s18 !=null && s17.equalsIgnoreCase(s18)) )
		    && ((WatchBox)de1).getWatchSymbolSize() ==  ((WatchBox)de2).getWatchSymbolSize()
		    && (int)((WatchBox)de1).getWatchSymbolWidth() ==  (int)((WatchBox)de2).getWatchSymbolWidth()
		    && ((WatchBox)de1).getWatchSymbolType().equalsIgnoreCase( ((WatchBox)de2).getWatchSymbolType())
		    && ((WatchBox)de1).getFillFlag() == ((WatchBox)de2).getFillFlag()
		    && ((WatchBox)de1).getBoxShape().equalsIgnoreCase( ((WatchBox)de2).getBoxShape())
		    && ((WatchBox)de1).getAnchors()[0].toString().substring(0, 3).equals( ((WatchBox)de2).getAnchors()[0].toString().substring(0, 3))
		    && ((WatchBox)de1).getAnchors()[1].toString().substring(0, 3).equals( ((WatchBox)de2).getAnchors()[1].toString().substring(0, 3)) 
			) {
			
			System.out.println("The "+deCollection +" #" +(m+1) +" "+ collection+" " + de1.getPgenCategory() + " in the first file is compared.");
			return true;
		}
		
		else {
			return false;
		}
	}

	private static Boolean compareLines(DrawableElement de1, DrawableElement de2, int m, String deCollection, String collection) {
		if (commonPoints(de1, de2, m) == false)
			return false;
		
		if (commonColors(de1, de2) == false)
			return false;
		
		if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType())
				&& ((Line) de1).isFilled().equals(((Line) de2).isFilled()) 			
				&& ((Line) de1).isClosedLine().equals(((Line) de2).isClosedLine())
				&& ((Line) de1).getSmoothFactor() == ((Line) de2).getSmoothFactor()	
				&& (int)de1.getLineWidth() == (int)de2.getLineWidth()	//&& de1.getSizeScale() == de2.getSizeScale() //normal line don't need it
				&& ((Line) de1).getFillPattern().toString().equalsIgnoreCase(((Line) de2).getFillPattern().toString())  
			) {
			
			System.out.println("The "+deCollection +" #" +(m+1) +" "+ collection+" " + de1.getPgenCategory() + " in the first file is compared.");
		    return true;
		}
		else {
			return false;
		}
	}

	private static Boolean compareFront(DrawableElement de1, DrawableElement de2, int m, String deCollection) {
		if (commonPoints(de1, de2, m) == false)
			return false;
		
		if (commonColors(de1, de2) == false)
			return false;
					
		if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType())
				&& ((Line)de1).isFilled().equals(((Line) de2).isFilled()) 				
				&& ((Line) de1).isClosedLine().equals(((Line) de2).isClosedLine())
				&& ((Line)de1).getSmoothFactor() == ((Line)de2).getSmoothFactor()	
				&& (int)de1.getLineWidth() == (int)de2.getLineWidth()	
				&& de1.getSizeScale() == de2.getSizeScale() //front no fillPattern
			) {
			
			System.out.println("The "+deCollection +" #" +(m+1) + " " + de1.getPgenCategory() + " in the first file is compared.");
		    return true;
		}
		else {
			return false;
		}
	}
	
	private static Boolean compareSymbol(DrawableElement de1, DrawableElement de2, int m, String deCollection, String collection) {
		if (commonPoints(de1, de2, m) == false)
			return false;
		
		if (commonColors(de1, de2) == false)
			return false;
		
		if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType()) 	
				&& ((SinglePointElement) de1).isClear() == ((SinglePointElement) de2).isClear()
				&& (int)de1.getLineWidth() == (int)de2.getLineWidth()	
				&& de1.getSizeScale() == de2.getSizeScale()			   
			) {
			System.out.println("The "+deCollection +" #" +(m+1) + " " + de1.getPgenCategory() + " in the first file is compared.");
			return true;
		}
		else {
			return false;
		}
	} 
	
	private static Boolean compareVector(DrawableElement de1, DrawableElement de2, int m, String deCollection, String collection) {
		if (commonPoints(de1, de2, m) == false)
			return false;
		
		if (commonColors(de1, de2) == false)
			return false;
		
		if ( !de1.getPgenType().equalsIgnoreCase("Hash")) { //hash is always clear=false
			if (((SinglePointElement) de1).isClear() != ((SinglePointElement)de2).isClear()) {				
				return false;
			}
		}
		
		if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType()) 
				&& (int)de1.getLineWidth() == (int)de2.getLineWidth()			
				&& de1.getSizeScale() == de2.getSizeScale()	
				&& ((Vector) de1).getDirection() - ((Vector) de2).getDirection() <0.00005       
				&& ((Vector) de1).getSpeed() == ((Vector) de2).getSpeed()
				&& ((Vector) de1).getArrowHeadSize() == ((Vector) de2).getArrowHeadSize() 
				&& ((Vector) de1).hasDirectionOnly() == ((Vector) de2).hasDirectionOnly()
			) {
			System.out.println("The "+deCollection +" #" +(m+1) +" "+ collection+" " + de1.getPgenCategory() + " in the first file is compared."); 
			return true;
		}
		else {
			return false;
		}
	}
	
	private static Boolean compareText(DrawableElement de1, DrawableElement de2, int m, String deCollection, String collection) {
		if ( !collection.startsWith("JET")) {    //JET's text shifted
			if (commonPoints(de1, de2, m) == false) 
				return false;	
		}
		
		if (commonColors(de1, de2) == false)
			return false;
		
		if (  de1.getPgenType().equalsIgnoreCase("General Text")) {
			String text1 ="", text2 ="";
			String[] s1 = ((Text)de1).getText();
			for (int i=0; i<s1.length; i++)
				text1 +=s1[i] + " ";
			String[] s2 = ((Text)de2).getText();
			for (int i=0; i<s2.length; i++)
				text2 +=s2[i] + " ";
			
			if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType()) 
				//&& de1.getFontSize() == de2.getFontSize()			&& de1.getFontName().equalsIgnoreCase(de2.getFontName())
				&& ((Text) de1).getStyle().toString().equalsIgnoreCase( ((Text) de2).getStyle().toString()) 
				&& ((Text) de1).getJustification().toString().equalsIgnoreCase( ((Text) de2).getJustification().toString())	
				&& ((Text) de1).getRotation() - ((Text) de2).getRotation() < 0.00005       
				&& ((Text) de1).getRotationRelativity().toString().equalsIgnoreCase( ((Text) de2).getRotationRelativity().toString())
				//&& de1.maskText() == de2.maskText()					&& de1.outlineText() == de1.outlineText() //for jetText error
				&& ((Text) de1).getXOffset() == ((Text) de2).getXOffset() 			
				&& ((Text) de1).getYOffset() == ((Text) de2).getYOffset()
				&& text1.equalsIgnoreCase(text2)
			) {
				
				System.out.println("The "+deCollection +" #" +(m+1) +" "+ collection+" " + de1.getPgenCategory() + " in the first file is compared.");
				return true;
			}
			else {
				return false;
			}
		}
		else if (  de1.getPgenType().equalsIgnoreCase("AVIATION_TEXT")) {
			if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType()) //&& de1.getColors()[0].equals(de2.getColors()[0])
					&& ((Text) de1).getFontSize() == ((Text) de2).getFontSize()			
					&& ((Text) de1).getFontName().equalsIgnoreCase(((Text) de2).getFontName())
					&& ((Text) de1).getStyle().toString().equalsIgnoreCase( ((Text) de2).getStyle().toString()) 
					&& ((AvnText) de1).getSymbolPatternName().equalsIgnoreCase( ((AvnText) de2).getSymbolPatternName())			
					&& ((AvnText) de1).getJustification().toString().equalsIgnoreCase( ((AvnText) de2).getJustification().toString())
					&& ((AvnText) de1).getBottomValue().equalsIgnoreCase( ((AvnText) de2).getBottomValue())	
					&& ((AvnText) de1).getTopValue().equalsIgnoreCase( ((AvnText) de2).getTopValue())
					&& ((AvnText) de1).getAvnTextType().toString().equalsIgnoreCase( ((AvnText) de2).getAvnTextType().toString())
				) {	
				
				System.out.println("The "+deCollection +" #" +(m+1) +" "+ collection+" " + de1.getPgenCategory() + " in the first file is compared.");
				return true;
				}
			else {
					return false;
			}
		}
		
		else if (  de1.getPgenType().equalsIgnoreCase("MID_LEVEL_CLOUD")) {
			if (  de1.getPgenType().equalsIgnoreCase(((Text) de2).getPgenType()) //&& de1.getColors()[0].equals(de2.getColors()[0])
					&& ((Text) de1).getFontSize() == ((Text) de2).getFontSize()			
					&& ((Text) de1).getFontName().equalsIgnoreCase(((Text) de2).getFontName())
					&& ((Text) de1).getStyle().toString().equalsIgnoreCase( ((Text) de2).getStyle().toString()) 	
					&& ((Text) de1).getJustification().toString().equalsIgnoreCase( ((Text) de2).getJustification().toString())
					&& ((MidCloudText)de1).getTstormLevels().equalsIgnoreCase( ((MidCloudText)de2).getTstormLevels())	
					&& ((MidCloudText)de1).getTstormTypes().equalsIgnoreCase( ((MidCloudText)de2).getTstormTypes())
					&& ((MidCloudText)de1).getIcingLevels().equalsIgnoreCase( ((MidCloudText)de2).getIcingLevels())	
					&& ((MidCloudText)de1).getIcingPattern().equalsIgnoreCase( ((MidCloudText)de2).getIcingPattern())
					&& ((MidCloudText)de1).getTurbulenceLevels().equalsIgnoreCase( ((MidCloudText)de2).getTurbulenceLevels())	
					&& ((MidCloudText)de1).getTurbulencePattern().equalsIgnoreCase( ((MidCloudText)de2).getTurbulencePattern())
					//&& ((MidCloudText)de1).getCloudAmounts().equalsIgnoreCase( ((MidCloudText)de2).getCloudAmounts())	
					&& ((MidCloudText)de1).getCloudTypes().equalsIgnoreCase( ((MidCloudText)de2).getCloudTypes())
				) {
				
				System.out.println("The "+deCollection +" #" +(m+1) +" "+ collection+" " + de1.getPgenCategory() + " in the first file is compared.");
				return true;
			}
			else {
				return false;
			}
		}
		return true;
	}
	
	private static Boolean compareCloudText(DrawableElement de1, DrawableElement de2, int m, String deCollection, String collection) {
		// different is that text is not compared, since general text is converted to midLevel
		if (commonPoints(de1, de2, m) == false)
			return false;		
		
		if (commonColors(de1, de2) == false)
			return false;
		
		if (  de1.getPgenType().equalsIgnoreCase("General Text")) {
			String text1 ="", text2 ="";
			String[] s1 = ((Text)de1).getText();
			for (int i=0; i<s1.length; i++)
				text1 +=s1[i] + " ";
			String[] s2 = ((Text)de2).getText();
			for (int i=0; i<s2.length; i++)
				text2 +=s2[i] + " ";
			
			if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType()) 
				&& ((Text) de1).getFontSize() == ((Text) de2).getFontSize()			
				&& ((Text) de1).getFontName().equalsIgnoreCase(((Text) de2).getFontName())
				&& ((Text) de1).getStyle().toString().equalsIgnoreCase( ((Text) de2).getStyle().toString()) 
				&& ((Text) de1).getJustification().toString().equalsIgnoreCase( ((Text) de2).getJustification().toString())	
				&& ((Text) de1).getRotation() - ((Text) de2).getRotation() < 0.00005       
				&& ((Text) de1).getRotationRelativity().toString().equalsIgnoreCase( ((Text) de2).getRotationRelativity().toString())
				//&& de1.maskText() == de2.maskText()					&& de1.outlineText() == de1.outlineText() //for jetText error
				&& ((Text) de1).getXOffset() == ((Text) de2).getXOffset() 			
				&& ((Text) de1).getYOffset() == ((Text) de2).getYOffset()
			) {
				
				System.out.println("The "+deCollection +" #" +(m+1) +" "+ collection+" " + de1.getPgenCategory() + " in the first file is compared.");
				return true;
			}
			else {
				return false;
			}
		}
		else if (  de1.getPgenType().equalsIgnoreCase("AVIATION_TEXT")) {
			if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType()) //&& de1.getColors()[0].equals(de2.getColors()[0])
					&& ((Text) de1).getFontSize() == ((Text) de2).getFontSize()			
					&& ((Text) de1).getFontName().equalsIgnoreCase(((Text) de2).getFontName())
					&& ((Text) de1).getStyle().toString().equalsIgnoreCase( ((Text) de2).getStyle().toString()) 
					&& ((Text) de1).getJustification().toString().equalsIgnoreCase( ((Text) de2).getJustification().toString())
					&& ((AvnText) de1).getSymbolPatternName().equalsIgnoreCase( ((AvnText) de2).getSymbolPatternName())			
					&& ((AvnText) de1).getBottomValue().equalsIgnoreCase( ((AvnText) de2).getBottomValue())	
					&& ((AvnText) de1).getTopValue().equalsIgnoreCase( ((AvnText) de2).getTopValue())
					&& ((AvnText) de1).getAvnTextType().toString().equalsIgnoreCase( ((AvnText) de2).getAvnTextType().toString())
				) {	
				
				System.out.println("The "+deCollection +" #" +(m+1) +" "+ collection+" " + de1.getPgenCategory() + " in the first file is compared.");
				return true;
				}
			else {
					return false;
			}
		}
		
		else if (  de1.getPgenType().equalsIgnoreCase("MID_LEVEL_CLOUD")) {
			if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType()) //&& de1.getColors()[0].equals(de2.getColors()[0])
					&& ((Text) de1).getFontSize() == ((Text) de2).getFontSize()			
					&& ((Text) de1).getFontName().equalsIgnoreCase(((Text) de2).getFontName())
					&& ((Text) de1).getStyle().toString().equalsIgnoreCase( ((Text) de2).getStyle().toString()) 	
					&& ((Text) de1).getJustification().toString().equalsIgnoreCase( ((Text) de2).getJustification().toString())
					&& ((MidCloudText)de1).getTstormLevels().equalsIgnoreCase( ((MidCloudText)de2).getTstormLevels())	
					&& ((MidCloudText)de1).getTstormTypes().equalsIgnoreCase( ((MidCloudText)de2).getTstormTypes())
					&& ((MidCloudText)de1).getCloudTypes().equalsIgnoreCase( ((MidCloudText)de2).getCloudTypes())
				) {
				
				System.out.println("The "+deCollection +" #" +(m+1) +" "+ collection+" " + de1.getPgenCategory() + " in the first file is compared.");
				return true;
			}
			else {
				return false;
			}
		}
		return true;
	}
	
	private static Boolean compareTrack(DrawableElement de1, DrawableElement de2, int m, String deCollection) {
		//compare InitialPointsnumbers, ExtrapPoints numbers and total points locations
		if ( ((Track)de1).getInitialPoints().length != ((Track)de2).getInitialPoints().length ) {
			return false;
		}
		if ( ((Track)de1).getExtrapPoints().length != ((Track)de2).getExtrapPoints().length ) {
			return false;
		}
		if (commonPoints(de1, de2, m) == false)
			return false;
		
		
		String trackCol1 = ((Track)de1).getInitialColor().toString();
		String trackCol2 = ((Track)de2).getInitialColor().toString();
		trackCol1 = trackCol1.substring(trackCol1.indexOf("[")+1, trackCol1.indexOf("]"));
		trackCol2 = trackCol2.substring(trackCol2.indexOf("[")+1, trackCol2.indexOf("]"));
		if (XmlUtil.getColorTag(trackCol1) != XmlUtil.getColorTag(trackCol2))
			return false;
		
		trackCol1 = ((Track)de1).getExtrapColor().toString();
		trackCol2 = ((Track)de2).getExtrapColor().toString();
		trackCol1 = trackCol1.substring(trackCol1.indexOf("[")+1, trackCol1.indexOf("]"));
		trackCol2 = trackCol2.substring(trackCol2.indexOf("[")+1, trackCol2.indexOf("]"));
		if (XmlUtil.getColorTag(trackCol1) != XmlUtil.getColorTag(trackCol2))
			return false;
		
		
		boolean[] b1 = ((Track)de1).getExtraPointTimeTextDisplayIndicator();
		boolean[] b2= ((Track)de2).getExtraPointTimeTextDisplayIndicator();
		String timeText1 = "", timeText2 = "";
		for (int i=0; i<b1.length; i++) {
			timeText1 += b1[i] + " ";
		}
		for (int i=0; i<b2.length; i++) {
			timeText2 += b2[i] + " ";
		}
		
		 
		if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType()) 
				&& (int)de1.getLineWidth() == (int)de2.getLineWidth()
				&& ((Track)de1).getSkipFactorTextString().equalsIgnoreCase(((Track)de2).getSkipFactorTextString()) 			
				&& ((Track)de1).isSetTimeButtonSelected() == ((Track)de2).isSetTimeButtonSelected()				
				&& ((Track)de1).getIntervalComboSelectedIndex() == ((Track)de2).getIntervalComboSelectedIndex()
				//&& ((Track)de1).getFontStyleComboSelectedIndex() ==  ((Track)de2).getFontStyleComboSelectedIndex()
				&& ((Track)de1).getFontSizeComboSelectedIndex() == ((Track)de2).getFontSizeComboSelectedIndex()
				&& ((Track)de1).getFontNameComboSelectedIndex() == ((Track)de2).getFontNameComboSelectedIndex()
				&& ((Track)de1).getIntervalTimeString().equalsIgnoreCase(((Track)de2).getIntervalTimeString())				
				&& ((Track)de1).getInitialMarker().equalsIgnoreCase(((Track)de2).getInitialMarker())
				&& ((Track)de1).getInitialLinePattern().equalsIgnoreCase(((Track)de2).getInitialLinePattern())
				//&& ((Track)de1).getFontStyle().toString().equalsIgnoreCase(((Track)de2).getFontStyle().toString())
				&& ((Track)de1).getFontSize() == ((Track)de2).getFontSize()
				&& ((Track)de1).getFontName().equalsIgnoreCase(((Track)de2).getFontName())
				&& ((Track)de1).getExtrapMarker().equalsIgnoreCase(((Track)de2).getExtrapMarker())
				&& ((Track)de1).getExtrapLinePattern().equalsIgnoreCase(((Track)de2).getExtrapLinePattern())
				&& ((Track)de1).getExtraPointTimeDisplayOption().toString().equalsIgnoreCase(((Track)de2).getExtraPointTimeDisplayOption().toString())				
				&& timeText1.equalsIgnoreCase(timeText1)				
			) {
			System.out.println("The "+deCollection +" #" +(m+1) + " " + de1.getPgenCategory() + " in the first file is compared.");
		    return true;
		}
		else {
			return false;
		}
	}
	private static Boolean compareSigmet(DrawableElement de1, DrawableElement de2, int m, String deCollection) {
		if (commonPoints(de1, de2, m) == false)
			return false;
		
		if (commonColors(de1, de2) == false)
			return false;
		
		if (((Line) de1).isFilled().equals("true")) 
			if ( !((Line) de1).getFillPattern().toString().equalsIgnoreCase(((Line) de2).getFillPattern().toString()) )
				return false;
		
		String s1 = ((Sigmet)de1).getEditableAttrLevelText2();
		String s2 = ((Sigmet)de2).getEditableAttrLevelText2(); //"null"
		String s3 = ((Sigmet)de1).getEditableAttrLevelText1();
		String s4 = ((Sigmet)de2).getEditableAttrLevelText1();
		String s5 = ((Sigmet)de1).getEditableAttrLevelInfo2();
		String s6 = ((Sigmet)de2).getEditableAttrLevelInfo2();
		String s7 = ((Sigmet)de1).getEditableAttrLevelInfo1();
		String s8 = ((Sigmet)de2).getEditableAttrLevelInfo1();
		String s9 = ((Sigmet)de1).getEditableAttrLevel();
		String s10 = ((Sigmet)de2).getEditableAttrLevel();
		String s11 = ((Sigmet)de1).getEditableAttrPhenomDirection();
		String s12 = ((Sigmet)de2).getEditableAttrPhenomDirection();
		String s13 = ((Sigmet)de1).getEditableAttrPhenomSpeed();
		String s14 = ((Sigmet)de2).getEditableAttrPhenomSpeed();
		String s15 = ((Sigmet)de1).getEditableAttrTrend();
		String s16 = ((Sigmet)de2).getEditableAttrTrend();
		String s17 = ((Sigmet)de1).getEditableAttrPhenom();
		String s18 = ((Sigmet)de2).getEditableAttrPhenom();
		String s19 = ((Sigmet)de1).getEditableAttrRemarks();
		String s20 = ((Sigmet)de2).getEditableAttrRemarks();
		String s21 = ((Sigmet)de1).getEditableAttrId();
		String s22 = ((Sigmet)de2).getEditableAttrId();
		String s23 = ((Sigmet)de1).getEditableAttrStatus();
		String s24 = ((Sigmet)de2).getEditableAttrStatus();
		String s25 = ((Sigmet)de1).getEditableAttrSeqNum();
		String s26 = ((Sigmet)de2).getEditableAttrSeqNum();
		String s27 = ((Sigmet)de1).getEditableAttrArea();
		String s28 = ((Sigmet)de2).getEditableAttrArea();
		String s29 = ((Sigmet)de1).getEditableAttrStatus();
		String s30 = ((Sigmet)de2).getEditableAttrStatus();
		String s31 = ((Sigmet)de1).getEditableAttrEndTime();
		String s32 = ((Sigmet)de2).getEditableAttrEndTime();
		String s33 = ((Sigmet)de1).getEditableAttrStartTime();
		String s34 = ((Sigmet)de2).getEditableAttrStartTime();

		
		if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType())
				&& ((Line) de1).isFilled().equals(((Line) de2).isFilled()) 			
				&& ((Line) de1).isClosedLine().equals(((Line) de2).isClosedLine())
				&& ((Line) de1).getSmoothFactor() == ((Line) de2).getSmoothFactor()	
				&& (int)de1.getLineWidth() == (int)de2.getLineWidth()			
				//&& de1.getSizeScale() == de2.getSizeScale() //vgf has no sizeScale
				//&& de1.getFillPattern().toString().equalsIgnoreCase(de2.getFillPattern().toString()) 
				//&& ((Sigmet)de1).getEditableAttrFromLine().equalsIgnoreCase(((Sigmet)de2).getEditableAttrFromLine())
				&& ( ( (s1==null || s1.equalsIgnoreCase("") || s1.equalsIgnoreCase("null")) && (s2==null || s2.equalsIgnoreCase("")) || s2.equalsIgnoreCase("null") ) 
		    		|| (s1 !=null && s2 !=null && s1.equalsIgnoreCase(s2)) )

		    	&& ( ((s3==null || s3.equalsIgnoreCase("") || s3.equalsIgnoreCase("null")) && (s4==null || s4.equalsIgnoreCase("")) || s4.equalsIgnoreCase("null") ) 
		    		|| (s3 !=null && s4 !=null && s3.equalsIgnoreCase(s4)) )
				&& ( ((s5==null || s5.equalsIgnoreCase("")) && (s6==null || s6.equalsIgnoreCase("")) ) 
		    		|| (s5 !=null && s6 !=null && s5.equalsIgnoreCase(s6)) )
		    	&& ( ((s7==null || s7.equalsIgnoreCase("")) && (s8==null || s8.equalsIgnoreCase("")) ) 
		    		|| (s7 !=null && s8 !=null && s7.equalsIgnoreCase(s8)) )
		    	&& ( ((s9==null || s9.equalsIgnoreCase("")) && (s10==null || s10.equalsIgnoreCase("")) ) 
		    		|| (s9 !=null && s10 !=null && s9.equalsIgnoreCase(s10)) )
				&& ( ((s11==null || s11.equalsIgnoreCase("")) && (s12==null || s12.equalsIgnoreCase("")) ) 
		    		|| (s11 !=null && s12 !=null && s11.equalsIgnoreCase(s12)) )
		    	&& ( ((s13==null || s13.equalsIgnoreCase("")) && (s14==null || s14.equalsIgnoreCase("")) ) 
		    		|| (s13 !=null && s14 !=null && s13.equalsIgnoreCase(s14)) )
		    	&& ( ((s15==null || s15.equalsIgnoreCase("")) && (s16==null || s16.equalsIgnoreCase("")) ) 
		    		|| (s15 !=null && s16 !=null && s15.equalsIgnoreCase(s16)) )
				&& ( ((s17==null || s17.equalsIgnoreCase("")) && (s18==null || s18.equalsIgnoreCase("")) ) 
		    		|| (s17 !=null && s18 !=null && s17.equalsIgnoreCase(s18)) )
		    	&& ( ((s19==null || s19.equalsIgnoreCase("")) && (s20==null || s20.equalsIgnoreCase("")) ) 
		    		|| (s19 !=null && s20 !=null && s19.equalsIgnoreCase(s20)) )
		    	&& ( ((s21==null || s21.equalsIgnoreCase("")) && (s22==null || s22.equalsIgnoreCase("")) ) 
		    		|| (s21 !=null && s22 !=null && s21.equalsIgnoreCase(s22)) )
				&& ( ((s23==null || s23.equalsIgnoreCase("") || s23.equalsIgnoreCase("-9999")) && (s24==null || s24.equalsIgnoreCase("")) || s24.equalsIgnoreCase("-9999") )
		    		|| (s23 !=null && s24 !=null && s23.equalsIgnoreCase(s24)) )
				&& ( ((s25==null || s25.equalsIgnoreCase("") || s25.equalsIgnoreCase("-9999")) && (s26==null || s26.equalsIgnoreCase("")) || s26.equalsIgnoreCase("-9999") ) 
		    		|| (s25 !=null && s26 !=null && s25.equalsIgnoreCase(s26)) )
				&& ( ((s27==null || s27.equalsIgnoreCase("")) && (s28==null || s28.equalsIgnoreCase("")) ) 
		    		|| (s27 !=null && s28 !=null && s27.equalsIgnoreCase(s28)) )
				&& ( ((s29==null || s29.equalsIgnoreCase("") || s29.equalsIgnoreCase("-9999")) && (s30==null || s30.equalsIgnoreCase("")) || s30.equalsIgnoreCase("-9999") ) 
		    		|| (s29 !=null && s30 !=null && s29.equalsIgnoreCase(s30)) )
				
		    	&& ( ((s31==null || s31.equalsIgnoreCase("")) && (s32==null || s32.equalsIgnoreCase("")) ) 
		    		|| (s31 !=null && s32 !=null && s31.equalsIgnoreCase(s32)) )
				&& ( ((s33==null || s33.equalsIgnoreCase("")) && (s34==null || s34.equalsIgnoreCase("")) ) 
		    		|| (s33 !=null && s34 !=null && s33.equalsIgnoreCase(s34)) )
				&& ((Sigmet)de1).getWidth() - ((Sigmet)de2).getWidth() <0.1  //91249.99
				&& ((Sigmet)de1).getType().equalsIgnoreCase(((Sigmet)de2).getType())
			) {

			System.out.println("The "+deCollection +" #" +(m+1) + " " + de1.getPgenCategory() + " in the first file is compared.");
		    return true;
		}
		else {
			return false;
		}
	}
	
	private static Boolean compareVolc(DrawableElement de1, DrawableElement de2, int m, String deCollection) {
		if (commonPoints(de1, de2, m) == false)
			return false;
		
		if (commonColors(de1, de2) == false)
			return false;
				
		if (((Volcano)de1).getName().equalsIgnoreCase(((Volcano)de2).getName())
				&& ((Volcano)de1).getProduct().equalsIgnoreCase(((Volcano)de2).getProduct())
				&& ((Volcano)de1).getSizeScale()==((Volcano)de2).getSizeScale()
				&& ((Volcano)de1).getLineWidth()==((Volcano)de2).getLineWidth()
				&& ((Volcano)de1).getNumber().equalsIgnoreCase(((Volcano)de2).getNumber())
				&& ((Volcano)de1).getTxtLoc().equalsIgnoreCase(((Volcano)de2).getTxtLoc())
				&& ((Volcano)de1).getArea().equalsIgnoreCase(((Volcano)de2).getArea())
				&& ((Volcano)de1).getOrigStnVAAC().equalsIgnoreCase(((Volcano)de2).getOrigStnVAAC())
				&& ((Volcano)de1).getWmoId().equalsIgnoreCase(((Volcano)de2).getWmoId())
				&& ((Volcano)de1).getHdrNum().equalsIgnoreCase(((Volcano)de2).getHdrNum())
				&& ((Volcano)de1).getElev().equalsIgnoreCase(((Volcano)de2).getElev())
				&& ((Volcano)de1).getYear().equalsIgnoreCase(((Volcano)de2).getYear())
				&& ((Volcano)de1).getAdvNum().equalsIgnoreCase(((Volcano)de2).getAdvNum())
				&& ((Volcano)de1).getCorr().equalsIgnoreCase(((Volcano)de2).getCorr())
				&& ((Volcano)de1).getInfoSource().equalsIgnoreCase(((Volcano)de2).getInfoSource())
				&& ((Volcano)de1).getAddInfoSource().equalsIgnoreCase(((Volcano)de2).getAddInfoSource())
				&& ((Volcano)de1).getAviColorCode().equalsIgnoreCase(((Volcano)de2).getAviColorCode())
				&& ((Volcano)de1).getErupDetails().equalsIgnoreCase(((Volcano)de2).getErupDetails())
				&& ((Volcano)de1).getObsAshDate().equalsIgnoreCase(((Volcano)de2).getObsAshDate())
				&& ((Volcano)de1).getObsAshTime().equalsIgnoreCase(((Volcano)de2).getObsAshTime())
				//&& ((Volcano)de1).getObsFcstAshCloudInfo().equalsIgnoreCase(((Volcano)de2).getObsFcstAshCloudInfo())
				&& ((Volcano)de1).getObsFcstAshCloudInfo6().equalsIgnoreCase(((Volcano)de2).getObsFcstAshCloudInfo6())
				&& ((Volcano)de1).getObsFcstAshCloudInfo12().equalsIgnoreCase(((Volcano)de2).getObsFcstAshCloudInfo12())
				&& ((Volcano)de1).getObsFcstAshCloudInfo18().equalsIgnoreCase(((Volcano)de2).getObsFcstAshCloudInfo18())
				&& ((Volcano)de1).getRemarks().equalsIgnoreCase(((Volcano)de2).getRemarks())
				&& ((Volcano)de1).getNextAdv().equalsIgnoreCase(((Volcano)de2).getNextAdv())  
				&& ((Volcano)de1).getForecasters().equalsIgnoreCase(((Volcano)de2).getForecasters())
				) {
		    return true;
		}
		else {
			return false;
		}
		
	}
	
	private static Boolean compareVolcAsh(DrawableElement de1, DrawableElement de2, int m, String deCollection) {
//		if (de1.isFilled().equals("true")) 
//		if ( !de1.getFillPattern().toString().equalsIgnoreCase(de2.getFillPattern().toString()) )
//			return false;
		if (commonPoints(de1, de2, m) == false)
			return false;
		
		if (commonColors(de1, de2) == false)
			return false;
		
		if (((Sigmet)de1).getType().equalsIgnoreCase(((Sigmet)de2).getType())
				&& ((Sigmet)de1).getLinePoints().length == ((Sigmet)de2).getLinePoints().length //getPoints.size()
				//&& ((Sigmet)de1).getLineWidth() == ((Sigmet)de2).getLineWidth()   //don't compare since tag2vgf error
				&& ((Sigmet)de1).getWidth() == ((Sigmet)de2).getWidth()
				&& ((Sigmet)de1).getEditableAttrFreeText().equalsIgnoreCase(((Sigmet)de2).getEditableAttrFreeText())
		) {
		    return true;
		}
		else {
			return false;
		}
	}
		
	private static Boolean compareGfa(DrawableElement de1, DrawableElement de2, int m, String deCollection) {
		if (commonPoints(de1, de2, m) == false)
			return false;
		
		String s1 = ((Gfa)de1).getGfaValue("RG");
		String s2 = ((Gfa)de2).getGfaValue("RG");
		String s3 = ((Gfa)de1).getGfaValue("Top");
		String s4 = ((Gfa)de2).getGfaValue("Top");
		String s5 = ((Gfa)de1).getGfaValue("Bottom");
		String s6 = ((Gfa)de2).getGfaValue("Bottom");
		String s7 = ((Gfa)de1).getGfaValue("Category");
		String s8 = ((Gfa)de2).getGfaValue("Category");
		String s9 = ((Gfa)de1).getGfaValue("Frequency");
		String s10 = ((Gfa)de2).getGfaValue("Frequency");
		String s11 = ((Gfa)de1).getGfaValue("Contour");
		String s12 = ((Gfa)de2).getGfaValue("Contour");
		String s13 = ((Gfa)de1).getGfaValue("Level");
		String s14 = ((Gfa)de2).getGfaValue("Level");
		String s15 = ((Gfa)de1).getGfaValue("Intensity");
		String s16 = ((Gfa)de2).getGfaValue("Intensity");
		String s17 = ((Gfa)de1).getGfaValue("FZL RANGE");
		String s18 = ((Gfa)de2).getGfaValue("FZL RANGE");
		String s19 = ((Gfa)de1).getGfaValue("Speed");
		String s20 = ((Gfa)de2).getGfaValue("Speed");
		String s21 = ((Gfa)de1).getGfaValue("DUE TO");
		String s22 = ((Gfa)de2).getGfaValue("DUE TO");
		String s23 = ((Gfa)de1).getGfaValue("Severity");
		String s24 = ((Gfa)de2).getGfaValue("Severity");
		String s25 = ((Gfa)de1).getGfaValue("Coverage");
		String s26 = ((Gfa)de2).getGfaValue("Coverage");
		String s27 = ((Gfa)de1).getGfaValue("FZL Top/Bottom");
		String s28 = ((Gfa)de2).getGfaValue("FZL Top/Bottom");
		
		if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType())
				&& ((Gfa)de1).getGfaHazard().equalsIgnoreCase(((Gfa)de2).getGfaHazard()	)				
				&& ((Gfa)de1).getGfaFcstHr().equalsIgnoreCase(((Gfa)de2).getGfaFcstHr()) 
				&& ((Gfa)de1).getGfaTag().equalsIgnoreCase(((Gfa)de2).getGfaTag()) 
				&& ((Gfa)de1).getGfaDesk().equalsIgnoreCase(((Gfa)de2).getGfaDesk()) 
				&& ((Gfa)de1).getGfaCycleDay() == ((Gfa)de2).getGfaCycleDay()
				&& ((Gfa)de1).getGfaCycleHour() == ((Gfa)de2).getGfaCycleHour() 
				&& ((Gfa)de1).getGfaIssueType().equalsIgnoreCase(((Gfa)de2).getGfaIssueType())	
				&& ((Gfa)de1).getGfaTextCoordinate().equals(((Gfa)de2).getGfaTextCoordinate())
				&& ((Gfa)de1).getGfaType().equalsIgnoreCase(((Gfa)de2).getGfaType()) 
				&& ((Gfa)de1).getGfaArea().equalsIgnoreCase( ((Gfa)de2).getGfaArea())
				&& ((Gfa)de1).getGfaStates().equalsIgnoreCase(((Gfa)de2).getGfaStates())
				&& ((Gfa)de1).getGfaBeginning() .equalsIgnoreCase(((Gfa)de2).getGfaBeginning())
				&& ((Gfa)de1).getGfaEnding().equalsIgnoreCase(((Gfa)de2).getGfaEnding())
				&&  (int)((Gfa)de1).getLineWidth() == (int)((Gfa)de2).getLineWidth() 
				
				&&  ( ( (s1==null || s1.equalsIgnoreCase("") || s1.equalsIgnoreCase("null")) && (s2==null || s2.equalsIgnoreCase("")) || s2.equalsIgnoreCase("null") ) 
			    		|| (s1 !=null && s2 !=null && s1.equalsIgnoreCase(s2)) )
				&&  ( ( (s3==null || s3.equalsIgnoreCase("") || s3.equalsIgnoreCase("null")) && (s4==null || s4.equalsIgnoreCase("")) || s4.equalsIgnoreCase("null") ) 
			    		|| (s3 !=null && s4 !=null && s3.equalsIgnoreCase(s4)) )
			    &&  ( ( (s5==null || s5.equalsIgnoreCase("") || s5.equalsIgnoreCase("null")) && (s6==null || s6.equalsIgnoreCase("")) || s6.equalsIgnoreCase("null") ) 
					    || (s5 !=null && s6 !=null && s5.equalsIgnoreCase(s6)) )
				&&  ( ( (s7==null || s7.equalsIgnoreCase("") || s7.equalsIgnoreCase("null")) && (s8==null || s8.equalsIgnoreCase("")) || s8.equalsIgnoreCase("null") ) 
			    		|| (s7 !=null && s8 !=null && s7.equalsIgnoreCase(s8)) )
				&&  ( ( (s9==null || s9.equalsIgnoreCase("") || s9.equalsIgnoreCase("null")) && (s10==null || s10.equalsIgnoreCase("")) || s10.equalsIgnoreCase("null") ) 
			    		|| (s9 !=null && s10 !=null && s9.equalsIgnoreCase(s10)) )
			    &&  ( ( (s11==null || s11.equalsIgnoreCase("") || s11.equalsIgnoreCase("null")) && (s12==null || s12.equalsIgnoreCase("")) || s12.equalsIgnoreCase("null") ) 
					    || (s11 !=null && s12 !=null && s11.equalsIgnoreCase(s12)) )
				&&  ( ( (s13==null || s13.equalsIgnoreCase("") || s13.equalsIgnoreCase("null")) && (s14==null || s14.equalsIgnoreCase("")) || s14.equalsIgnoreCase("null") ) 
			    		|| (s13 !=null && s14 !=null && s13.equalsIgnoreCase(s14)) )
				&&  ( ( (s15==null || s15.equalsIgnoreCase("") || s15.equalsIgnoreCase("null")) && (s16==null || s16.equalsIgnoreCase("")) || s16.equalsIgnoreCase("null") ) 
			    		|| (s15 !=null && s16 !=null && s15.equalsIgnoreCase(s16)) )
			    &&  ( ( (s17==null || s17.equalsIgnoreCase("") || s17.equalsIgnoreCase("null")) && (s18==null || s18.equalsIgnoreCase("")) || s18.equalsIgnoreCase("null") ) 
					    || (s17 !=null && s18 !=null && s17.equalsIgnoreCase(s18)) )
			    &&  ( ( (s19==null || s19.equalsIgnoreCase("") || s19.equalsIgnoreCase("null")) && (s20==null || s20.equalsIgnoreCase("")) || s20.equalsIgnoreCase("null") ) 
			    		|| (s19 !=null && s20 !=null && s19.equalsIgnoreCase(s20)) )
				&&  ( ( (s21==null || s21.equalsIgnoreCase("") || s21.equalsIgnoreCase("null")) && (s22==null || s22.equalsIgnoreCase("")) || s22.equalsIgnoreCase("null") ) 
			    		|| (s21 !=null && s22 !=null && s21.equalsIgnoreCase(s22)) )
			    &&  ( ( (s23==null || s23.equalsIgnoreCase("") || s23.equalsIgnoreCase("null")) && (s24==null || s24.equalsIgnoreCase("")) || s24.equalsIgnoreCase("null") ) 
					    || (s23 !=null && s24 !=null && s23.equalsIgnoreCase(s24)) )
				&&  ( ( (s25==null || s25.equalsIgnoreCase("") || s25.equalsIgnoreCase("null")) && (s26==null || s26.equalsIgnoreCase("")) || s26.equalsIgnoreCase("null") ) 
			    		|| (s25 !=null && s26 !=null && s25.equalsIgnoreCase(s26)) )
				&&  ( ( (s27==null || s27.equalsIgnoreCase("") || s27.equalsIgnoreCase("null")) && (s28==null || s28.equalsIgnoreCase("")) || s28.equalsIgnoreCase("null") ) 
			    		|| (s27 !=null && s28 !=null && s27.equalsIgnoreCase(s28)) )
			    
			) {
			System.out.println("The "+deCollection +" #" +(m+1) + " " + de1.getPgenCategory() + " in the first file is compared. ");
			return true;
		}
		else {
			return false;
		}
	}
	
	private static Boolean compareTca(DrawableElement de1, DrawableElement de2, int m, String deCollection) {
		if (commonPoints(de1, de2, m) == false)
			return false;
		
		SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy/HHmm"); //don't compare seconds
		sdf.setTimeZone( TimeZone.getTimeZone("GMT"));
		
		Calendar c1 = ((TCAElement)de1).getAdvisoryTime();
		Calendar c2 = ((TCAElement)de2).getAdvisoryTime();
		if ((c1 != null && c2 == null) || (c1 == null && c2 != null) ) {
			return false;
		}
		else if (c1 != null && c2 != null) {
			
			if ( !sdf.format( c1.getTime()).equalsIgnoreCase( sdf.format( c2.getTime()) ))
				return false;
		}
		
		ArrayList<TropicalCycloneAdvisory> list1 = ((TCAElement)de1).getAdvisories(); 
		ArrayList<TropicalCycloneAdvisory> list2 = ((TCAElement)de1).getAdvisories(); 
		if (list1.size() != list2.size())
			return false;
		
		for (int i=0; i<list1.size(); i++) {
			if ( !list1.get(i).getSeverity().equalsIgnoreCase(list2.get(i).getSeverity()) )
				return false;
			if ( !list1.get(i).getAdvisoryType().equalsIgnoreCase(list2.get(i).getAdvisoryType()) )
				return false;
			if ( !list1.get(i).getGeographyType().equalsIgnoreCase(list2.get(i).getGeographyType()) )
				return false;
			
//			BPGeography geo1 = list1.get(i).getSegment(); breakpoint name won't exist in vgf, so don't compare
		}
		
		
		if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType())
				&& ((TCAElement)de1).getTextLocation().equals(((TCAElement)de2).getTextLocation()) 			
				&& ((TCAElement)de1).getTimeZone().equals(((TCAElement)de2).getTimeZone())
				&& ((TCAElement)de1).getAdvisoryNumber().equalsIgnoreCase(((TCAElement)de2).getAdvisoryNumber()	)
				
				&& ((TCAElement)de1).getStormType().equalsIgnoreCase(((TCAElement)de2).getStormType()) 
				&& ((TCAElement)de1).getIssueStatus().equalsIgnoreCase(((TCAElement)de2).getIssueStatus()) 
				&& ((TCAElement)de1).getBasin().equalsIgnoreCase(((TCAElement)de2).getBasin()) 
				&& ((TCAElement)de1).getStormName().equalsIgnoreCase(((TCAElement)de2).getStormName()) 
				&& ((TCAElement)de1).getStormNumber() == ((TCAElement)de2).getStormNumber()								
			) {
			
			System.out.println("The "+deCollection +" #" +(m+1) + " " + de1.getPgenCategory() + " in the first file is compared.");
			return true;
		}
		else {
			return false;
		}
	}
	
	
	private static Boolean compareArc(DrawableElement de1, DrawableElement de2, int m, String deCollection) {
		if (commonPoints(de1, de2, m) == false)
			return false;
		
		if (commonColors(de1, de2) == false)
			return false;
		 		
		//endAngle="360.0" startAngle="0.0" axisRatio="1.0"

		if (  de1.getPgenType().equalsIgnoreCase(de2.getPgenType())
				&& ((Line) de1).isFilled().equals(((Line) de2).isFilled()) 			
				&& ((Line) de1).isClosedLine().equals(((Line) de2).isClosedLine())
				&& ((Line) de1).getSmoothFactor() == ((Line) de2).getSmoothFactor()	
				&& (int)de1.getLineWidth() == (int)de2.getLineWidth() 	
				&& ((Line) de1).getSizeScale() == ((Line) de2).getSizeScale()
				&& ((Line) de1).getFillPattern().toString().equalsIgnoreCase(((Line) de2).getFillPattern().toString())  
			) {
			
			System.out.println("The "+deCollection +" #" +(m+1) + " " + de1.getPgenCategory() + " in the first file is compared.");
			return true;
		}
		else {
			return false;
		}
	}
	
	public static Properties load(File propsFile) throws IOException {
        Properties props = new Properties();
        FileInputStream fis = new FileInputStream(propsFile);
        props.load(fis); //.loadFromXML(fis);
        String s = props.getProperty("HTTPServer");
        fis.close();
        return props;
    }
	
	public String foo(boolean value){
		if(value) {
			return "yes";
		} else {
			return "no";
		}
	}
	
	public int sum(int ... vals){
		int sum= 0;
		for(int i: vals) {
			sum += i -i + i + 100;
		}
		sum -= (vals.length * 100);
		return sum;
	}
	
	
	public static void main(String[] args) throws IOException { 
		if (!new File(args[0]).exists()) {
			System.out.println("The original file does not exist.\n");
			return;
		}
		if (!new File(args[1]).exists()) {
			System.out.println("The converted file does not exist.\n");
			return;
		}
		
		new TestXmlConvert().testXml(args[0], args[1]);
		//new TestXmlConvert().testXml("/usr1/qzhou/to11d6/works/data1/outlook.xml", "/usr1/qzhou/to11d6/works/data1/outlookA.xml");
	}
}




