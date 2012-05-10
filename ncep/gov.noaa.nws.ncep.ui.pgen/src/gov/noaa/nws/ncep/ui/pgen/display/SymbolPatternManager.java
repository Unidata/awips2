/*
 * SymbolPatternManager
 * 
 * Date created: 10 DECEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;

import java.io.File;
import java.io.PrintWriter;
import java.util.HashMap;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class maintains a HashMap of defined Symbol Patterns that can be applied to
 * a displayable symbol at a given location.<P>
 * This class is implemented as a singleton, and the predefined Symbol Patterns are
 * constructed when the instance is created.  After the internal patterns are loaded, others
 * can be read in from an XML file if it exists.
 * 
 * Users can get a reference to this object using the static method getInstance().
 * @author sgilbert
 *
 */
public class SymbolPatternManager {

	/**
	 * The singleton instance;
	 */
	private static SymbolPatternManager instance=null;;
	
	/**
	 * A map of the available Symbol Patterns with a descriptive String as the key. 
	 */
	private HashMap<String,SymbolPattern> patternMap;
	
	/**
	 * constructor used by the getInstance method.
	 */
	protected SymbolPatternManager() {
		
		patternMap = new HashMap<String,SymbolPattern>();
		initialize();
		
	}
	
	/**
	 * Static method used to request the instance of the SymbolPatternList object.
	 * @return reference to this object
	 */
	public static synchronized SymbolPatternManager getInstance() {
		
		if ( instance == null ) {
			instance = new SymbolPatternManager();
		}
		return instance;	
				
	}
	
	/**
	 * Initialize the HashMap holding the Symbol Patterns
	 */
	private void initialize() {
	
		loadInternal();
		File patterns = PgenStaticDataProvider.getProvider().getStaticFile( 
				PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + "symbolPatterns.xml");
		if ( patterns != null && patterns.exists() ) {
			loadPatternsFromFile( patterns.getAbsolutePath() );
		}				
	}
	
	/**
	 * Gets the SymbolPattern associated with the requested String
	 * @param key the name/description of the SymbolPattern desired.
	 * @return The requested SymbolPattern
	 * @throws if requested pattern cannot be found.
	 */
	public SymbolPattern getSymbolPattern(String key) throws SymbolPatternException {
		
		SymbolPattern pattern = patternMap.get(key);
		if ( pattern == null ) throw new SymbolPatternException("Could not find symbol pattern: " + key);
		return pattern;
	}

	/**
	 * Gets a list of all currently available SymbolPatterns
	 * @return An array of Symbol Pattern names
	 */
	public String[] getPatternNames() {
		
		String[] names = new String[patternMap.size()];
		
		int i=0;
		for ( SymbolPattern sp : patternMap.values() ) {
			names[i++] = sp.getName();
		}
		return names;
	}
	
	/**
	 * Gets a list of IDs used to identify and request a specific LinePattern
	 * @return An array of Pattern ids
	 */
	public String[] getPatternIds() {
		
		String[] ids = new String[patternMap.size()];
		
		int i=0;
		for ( String str : patternMap.keySet() ) {
			ids[i++] = str;
		}
		return ids;
		
	}
	
	/**
	 * Constructs the HashMap holding all these internally defined SymbolPatterns.
	 * 
	 */
	private void loadInternal() {
		
		SymbolPattern sp;
	
		/*
		 * Marker Section
		 */
		sp = new SymbolPattern("Plus Sign");
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,0.0), new Coordinate(3.0,0.0) });
		sp.addPath(new Coordinate[] {new Coordinate(0.0,3.0), new Coordinate(0.0,-3.0) });
		patternMap.put("PLUS_SIGN", sp);

		sp = new SymbolPattern("Octagon");
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,-3.0), new Coordinate(1.0,-3.0),
				                     new Coordinate(3.0,-1.0), new Coordinate(3.0,1.0),
				                     new Coordinate(1.0,3.0), new Coordinate(-1.0,3.0),
				                     new Coordinate(-3.0,1.0), new Coordinate(-3.0,-1.0),
									 new Coordinate(-1.0,-3.0)  });
		patternMap.put("OCTAGON", sp);

		sp = new SymbolPattern("Triangle");
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-3.0), new Coordinate(3.0,-3.0),
				                     new Coordinate(0.0,3.0), new Coordinate(-3.0,-3.0) });
		patternMap.put("TRIANGLE", sp);

		sp = new SymbolPattern("Box");
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-3.0), new Coordinate(3.0,-3.0),
				                     new Coordinate(3.0,3.0), new Coordinate(-3.0,3.0),
									 new Coordinate(-3.0,-3.0)  });
		patternMap.put("BOX", sp);

		sp = new SymbolPattern("Small X");
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,-2.0), new Coordinate(2.0,2.0) });
		sp.addPath(new Coordinate[] {new Coordinate(2.0,-2.0), new Coordinate(-2.0,2.0) });
		patternMap.put("SMALL_X", sp);

		sp = new SymbolPattern("Diamond");
		sp.addPath(new Coordinate[] {new Coordinate(0.0,-3.0), new Coordinate(-3.0,0.0),
				                     new Coordinate(0.0,3.0), new Coordinate(3.0,0.0),
									 new Coordinate(0.0,-3.0)  });
		patternMap.put("DIAMOND", sp);

		sp = new SymbolPattern("Up Arrow");
		sp.addPath(new Coordinate[] {new Coordinate(0.0,-3.0), new Coordinate(0.0,0.0),
					 				 new Coordinate(-3.0,0.0), new Coordinate(0.0,3.0),
					 				 new Coordinate(3.0,0.0), new Coordinate(0.0,0.0)  });
		patternMap.put("UP_ARROW", sp);

		sp = new SymbolPattern("Bar X");
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-3.0), new Coordinate(3.0,3.0),
				                     new Coordinate(-3.0,3.0), new Coordinate(3.0,-3.0) });
		patternMap.put("X_WITH_TOP_BAR", sp);

		sp = new SymbolPattern("Letter Z");
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,3.0), new Coordinate(3.0,3.0),
				                     new Coordinate(-3.0,-3.0), new Coordinate(3.0,-3.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,0.0), new Coordinate(2.0,0.0) });
		patternMap.put("Z_WITH_BAR", sp);

		sp = new SymbolPattern("Letter Y");
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,3.0), new Coordinate(0.0,0.0),
				                     new Coordinate(0.0,-3.0) });
		sp.addPath(new Coordinate[] {new Coordinate(0.0,0.0), new Coordinate(3.0,3.0) });
		patternMap.put("Y", sp);

		sp = new SymbolPattern("Box X");
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,-1.0), new Coordinate(1.0,-1.0),
				 					 new Coordinate(1.0,1.0), new Coordinate(-1.0,1.0),
				                     new Coordinate(-1.0,-1.0), new Coordinate(-3.0,-3.0) });
		sp.addPath(new Coordinate[] {new Coordinate(1.0,-1.0), new Coordinate(3.0,-3.0) });
		sp.addPath(new Coordinate[] {new Coordinate(1.0,1.0), new Coordinate(3.0,3.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,1.0), new Coordinate(-3.0,3.0) });
		patternMap.put("BOX_WITH_DIAGONALS", sp);

		sp = new SymbolPattern("Asterisk");
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,-2.0), new Coordinate(2.0,2.0) });
		sp.addPath(new Coordinate[] {new Coordinate(2.0,-2.0), new Coordinate(-2.0,2.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,0.0), new Coordinate(3.0,0.0) });
		sp.addPath(new Coordinate[] {new Coordinate(0.0,-3.0), new Coordinate(0.0,3.0) });
		patternMap.put("ASTERISK", sp);

		sp = new SymbolPattern("Hourglass");
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-3.0), new Coordinate(3.0,3.0),
					 				 new Coordinate(-3.0,3.0), new Coordinate(3.0,-3.0),
					 				 new Coordinate(-3.0,-3.0)  });
		patternMap.put("HOURGLASS_X", sp);

		sp = new SymbolPattern("Star");
		sp.addPath(new Coordinate[] {new Coordinate(0.0,3.0), new Coordinate(-1.0,1.0),
					 				 new Coordinate(-3.0,1.0), new Coordinate(-1.0,0.0),
					 				 new Coordinate(-2.0,-2.0), new Coordinate(0.0,-1.0),
					 				 new Coordinate(2.0,-2.0), new Coordinate(1.0,0.0),
					 				 new Coordinate(3.0,1.0), new Coordinate(1.0,1.0),
					 				 new Coordinate(0.0,3.0)  });
		patternMap.put("STAR", sp);

		sp = new SymbolPattern("Dot");
		sp.addPath(new Coordinate[] {new Coordinate(0.5,0.0), new Coordinate(0.35,0.35),
                					 new Coordinate(0.0,0.5), new Coordinate(-0.35,0.35),
                					 new Coordinate(-0.5,0.0), new Coordinate(-0.35,-0.35),
                					 new Coordinate(0.0,-0.5), new Coordinate(0.35,-0.35),
                					 new Coordinate(0.5,0.0)  }, true );
		patternMap.put("DOT", sp);

		sp = new SymbolPattern("Large X");
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-3.0), new Coordinate(3.0,3.0) });
		sp.addPath(new Coordinate[] {new Coordinate(3.0,-3.0), new Coordinate(-3.0,3.0) });
		patternMap.put("LARGE_X", sp);

		sp = new SymbolPattern("Filled Octagon");
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,-3.0), new Coordinate(1.0,-3.0),
				                     new Coordinate(3.0,-1.0), new Coordinate(3.0,1.0),
				                     new Coordinate(1.0,3.0), new Coordinate(-1.0,3.0),
				                     new Coordinate(-3.0,1.0), new Coordinate(-3.0,-1.0),
									 new Coordinate(-1.0,-3.0)  }, true );
		patternMap.put("FILLED_OCTAGON", sp);

		sp = new SymbolPattern("Filled Triangle");
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-3.0), new Coordinate(3.0,-3.0),
				                     new Coordinate(0.0,3.0), new Coordinate(-3.0,-3.0) }, true );
		patternMap.put("FILLED_TRIANGLE", sp);

		sp = new SymbolPattern("Filled Box");
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-3.0), new Coordinate(3.0,-3.0),
				                     new Coordinate(3.0,3.0), new Coordinate(-3.0,3.0),
									 new Coordinate(-3.0,-3.0)  }, true );
		patternMap.put("FILLED_BOX", sp);

		sp = new SymbolPattern("Filled Diamond");
		sp.addPath(new Coordinate[] {new Coordinate(0.0,-3.0), new Coordinate(-3.0,0.0),
				                     new Coordinate(0.0,3.0), new Coordinate(3.0,0.0),
									 new Coordinate(0.0,-3.0)  }, true );
		patternMap.put("FILLED_DIAMOND", sp);

		sp = new SymbolPattern("Filled Star");
		sp.addPath(new Coordinate[] {new Coordinate(0.0,3.0), new Coordinate(-1.0,1.0),
					 				 new Coordinate(-3.0,1.0), new Coordinate(-1.0,0.0),
					 				 new Coordinate(-2.0,-2.0), new Coordinate(0.0,-1.0),
					 				 new Coordinate(2.0,-2.0), new Coordinate(1.0,0.0),
					 				 new Coordinate(3.0,1.0), new Coordinate(1.0,1.0),
					 				 new Coordinate(0.0,3.0)  }, true );
		patternMap.put("FILLED_STAR", sp);

		sp = new SymbolPattern("Minus Sign");
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,0.0), new Coordinate(3.0,0.0) });
		patternMap.put("MINUS_SIGN", sp);

		/*
		 * Wx Symbol Section
		 */
		sp = new SymbolPattern("Haze");     //Wx005
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-1.0), new Coordinate(-3.0,0.0),
									 new Coordinate(-2.0,1.0), new Coordinate(-1.0,1.0),
									 new Coordinate(2.0,-2.0), new Coordinate(3.0,-2.0),
									 new Coordinate(4.0,-1.0), new Coordinate(4.0,0.0),
									 new Coordinate(3.0,1.0), new Coordinate(2.0,1.0),
									 new Coordinate(-1.0,-2.0), new Coordinate(-2.0,-2.0),
									 new Coordinate(-3.0,-1.0)  });
		patternMap.put("PRESENT_WX_005", sp);

		sp = new SymbolPattern("Light Fog");     //Wx10
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-1.0), new Coordinate(3.0,-1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,1.0), new Coordinate(3.0,1.0) });
		patternMap.put("PRESENT_WX_010", sp);

		sp = new SymbolPattern("Fog, Sky not discernible");     //Wx45
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-2.0), new Coordinate(3.0,-2.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,0.0), new Coordinate(3.0,0.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,2.0), new Coordinate(3.0,2.0) });
		patternMap.put("PRESENT_WX_045", sp);

		sp = new SymbolPattern("Continuous drizzle, slight at observation time");     //Wx51
		sp.addDot(new Coordinate(-2.0,1.0), 0.75);
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,1.0), new Coordinate(-2.0,0.0),
				 new Coordinate(-3.0,-1.0)  });
		sp.addDot(new Coordinate(2.0,1.0), 0.75);
		sp.addPath(new Coordinate[] {new Coordinate(2.0,1.0), new Coordinate(2.0,0.0),
				 new Coordinate(1.0,-1.0)  });
		patternMap.put("PRESENT_WX_051", sp);

		sp = new SymbolPattern("Slight freezing drizzle");     //Wx56
		sp.addDot(new Coordinate(-3.0,1.0), 0.75);
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,1.0), new Coordinate(-3.0,-0.0),
				                     new Coordinate(-4.0,-1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,-2.0), new Coordinate(-5.0,-1.0),
				 new Coordinate(-5.0,0.0), new Coordinate(-5.0,1.0),
				 new Coordinate(-4.0,2.0), new Coordinate(-2.0,2.0),
				 new Coordinate(2.0,-2.0), new Coordinate(4.0,-2.0),
				 new Coordinate(5.0,-1.0), new Coordinate(5.0,1.0),
				 new Coordinate(4.0,2.0)  });
		patternMap.put("PRESENT_WX_056", sp);

		sp = new SymbolPattern("Continuous rain");     //Wx61
		sp.addDot(new Coordinate(-2.0,0.0), 1.10);
		sp.addDot(new Coordinate(2.0,0.0), 1.10);
		patternMap.put("PRESENT_WX_061", sp);

		sp = new SymbolPattern("Continuous moderate rain");     //Wx63
		sp.addDot(new Coordinate(-2.0,-1.0), 1.10);
		sp.addDot(new Coordinate(2.0,-1.0), 1.10);
		sp.addDot(new Coordinate(0.0,2.0), 1.10);
		patternMap.put("PRESENT_WX_063", sp);

		sp = new SymbolPattern("Continuous heavy rain");     //Wx65
		sp.addDot(new Coordinate(0.0,-3.0), 1.10);
		sp.addDot(new Coordinate(-2.0,0.0), 1.10);
		sp.addDot(new Coordinate(2.0,0.0), 1.10);
		sp.addDot(new Coordinate(0.0,3.0), 1.10);
		patternMap.put("PRESENT_WX_065", sp);

		sp = new SymbolPattern("Slight freezing rain");     //Wx66
		sp.addDot(new Coordinate(-3.0,0.0), 1.10);
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,-2.0), new Coordinate(-5.0,-1.0),
				 new Coordinate(-5.0,0.0), new Coordinate(-5.0,1.0),
				 new Coordinate(-4.0,2.0), new Coordinate(-2.0,2.0),
				 new Coordinate(2.0,-2.0), new Coordinate(4.0,-2.0),
				 new Coordinate(5.0,-1.0), new Coordinate(5.0,1.0),
				 new Coordinate(4.0,2.0)  });
		patternMap.put("PRESENT_WX_066", sp);

		sp = new SymbolPattern("Continuous Light Snow");     //Wx71
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,-1.0), new Coordinate(-2.0,1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-2.0,-1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,0.0), new Coordinate(-1.0,0.0) });
		sp.addPath(new Coordinate[] {new Coordinate(2.0,-1.0), new Coordinate(4.0,1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(2.0,1.0), new Coordinate(4.0,-1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(1.0,0.0), new Coordinate(5.0,0.0) });
		patternMap.put("PRESENT_WX_071", sp);

		sp = new SymbolPattern("Moderate Snow");     //Wx73
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,0.0), new Coordinate(-2.0,-2.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,-2.0), new Coordinate(-2.0,0.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,-1.0), new Coordinate(-1.0,-1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(2.0,0.0), new Coordinate(4.0,-2.0) });
		sp.addPath(new Coordinate[] {new Coordinate(2.0,-2.0), new Coordinate(4.0,0.0) });
		sp.addPath(new Coordinate[] {new Coordinate(1.0,-1.0), new Coordinate(5.0,-1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,3.0), new Coordinate(1.0,1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,1.0), new Coordinate(1.0,3.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,2.0), new Coordinate(2.0,2.0) });
		patternMap.put("PRESENT_WX_073", sp);

		sp = new SymbolPattern("Continuous Heavy Snow");     //Wx75
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,4.0), new Coordinate(1.0,2.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,2.0), new Coordinate(1.0,4.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,3.0), new Coordinate(2.0,3.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-2.0,-1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,-1.0), new Coordinate(-2.0,1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,0.0), new Coordinate(-1.0,0.0) });
		sp.addPath(new Coordinate[] {new Coordinate(2.0,1.0), new Coordinate(4.0,-1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(2.0,-1.0), new Coordinate(4.0,1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(1.0,0.0), new Coordinate(5.0,0.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,-2.0), new Coordinate(1.0,-4.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,-4.0), new Coordinate(1.0,-2.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,-3.0), new Coordinate(2.0,-3.0) });
		patternMap.put("PRESENT_WX_075", sp);

		sp = new SymbolPattern("Ice pellets");     //Wx79
		sp.addDot(new Coordinate(0.0,0.0), 0.75);
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-2.0), new Coordinate(0.0,3.0),
				 new Coordinate(3.0,-2.0), new Coordinate(-3.0,-2.0) });
		patternMap.put("PRESENT_WX_079", sp);

		sp = new SymbolPattern("Slight rain shower");     //Wx80
		sp.addDot(new Coordinate(0.0,3.0), 0.75);
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,1.0), new Coordinate(0.0,-5.0),
				 new Coordinate(3.0,1.0), new Coordinate(-3.0,1.0) });
		patternMap.put("PRESENT_WX_080", sp);

		sp = new SymbolPattern("Slight Snow Showers");     //Wx85
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,1.0), new Coordinate(0.0,-5.0),
									 new Coordinate(3.0,1.0), new Coordinate(-3.0, 1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,3.0), new Coordinate(1.0,5.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,5.0), new Coordinate(1.0,3.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,4.0), new Coordinate(2.0,4.0) });
		patternMap.put("PRESENT_WX_085", sp);

		sp = new SymbolPattern("Moderate Hail Showers");     //Wx88
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,1.0), new Coordinate(0.0,-5.0),
				                     new Coordinate(3.0, 1.0), new Coordinate(-3.0,1.0) }, false );
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,2.0), new Coordinate(0.0,4.0),
									 new Coordinate(3.0, 2.0), new Coordinate(-3.0,2.0) }, false );
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,-1.0), new Coordinate(2.0,-1.0) }, false );
		patternMap.put("PRESENT_WX_088", sp);

		sp = new SymbolPattern("Slight Shower of Hail");     //Wx89
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,1.0), new Coordinate(0.0,-5.0),
				                     new Coordinate(3.0, 1.0), new Coordinate(-3.0,1.0) }, false );
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,2.0), new Coordinate(0.0,4.0),
									 new Coordinate(3.0, 2.0), new Coordinate(-3.0,2.0) }, true );
		sp.addPath(new Coordinate[] {new Coordinate(0.0,4.0), new Coordinate(0.0,2.0) }, false );
		patternMap.put("PRESENT_WX_089", sp);

		sp = new SymbolPattern("Slight or mod thinderstorm with rain");     //Wx95
		sp.addDot(new Coordinate(0.0,3.0), 1.15);
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,1.0), new Coordinate(2.0,1.0),
				 new Coordinate(0.0,-1.0), new Coordinate(3.0,-4.0),
				 new Coordinate(3.0,-3.0), new Coordinate(3.0,-4.0),
				 new Coordinate(2.0,-4.0)  });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,-4.0), new Coordinate(-1.0,1.0) }, false );
		patternMap.put("PRESENT_WX_095", sp);

		sp = new SymbolPattern("Slight or Mod Thunderstorm with Snow");     //Wx105
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,1.0), new Coordinate(2.0,1.0),
									 new Coordinate(0.0,-1.0), new Coordinate(3.0,-4.0),
									 new Coordinate(3.0,-3.0), new Coordinate(3.0,-4.0),
									 new Coordinate(2.0,-4.0)  });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,-4.0), new Coordinate(-1.0,1.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,2.0), new Coordinate(1.0,4.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,4.0), new Coordinate(1.0,2.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,3.0), new Coordinate(2.0,3.0) });
		patternMap.put("PRESENT_WX_105", sp);

		sp = new SymbolPattern("Volcanic activity");     //Wx201
		sp.addDot(new Coordinate(0.0,-4.0), 0.75);
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,-4.0), new Coordinate(-4.0,-4.0),
				 new Coordinate(-2.0,1.0), new Coordinate(2.0,1.0),
				 new Coordinate(4.0,-4.0), new Coordinate(1.0,-4.0)  });
		sp.addPath(new Coordinate[] {new Coordinate(0.0,1.0), new Coordinate(-2.0,4.0) }, false );
		sp.addPath(new Coordinate[] {new Coordinate(0.0,1.0), new Coordinate(0.0,4.0) }, false );
		sp.addPath(new Coordinate[] {new Coordinate(0.0,1.0), new Coordinate(2.0,4.0) }, false );
		patternMap.put("PRESENT_WX_201", sp);

		/*
		 * Special Symbols Section
		 */
		sp = new SymbolPattern("High Pressure H");     //Sp12
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,4.0), new Coordinate(-2.0,4.0),
									 new Coordinate(-2.0,1.0), new Coordinate(2.0,1.0),
									 new Coordinate(2.0,4.0), new Coordinate(4.0,4.0),
									 new Coordinate(4.0,-5.0), new Coordinate(2.0,-5.0),
									 new Coordinate(2.0,-1.0), new Coordinate(-2.0,-1.0),
									 new Coordinate(-2.0,-5.0), new Coordinate(-4.0,-5.0),
									 new Coordinate(-4.0,4.0)  }, true );
		patternMap.put("HIGH_PRESSURE_H", sp);

		sp = new SymbolPattern("Low Pressure L");     //Sp13
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,4.0), new Coordinate(-2.0,4.0),
									 new Coordinate(-2.0,-3.0), new Coordinate(4.0,-3.0),
									 new Coordinate(4.0,-5.0), new Coordinate(-4.0,-5.0),
									 new Coordinate(-4.0,4.0)  }, true );
		patternMap.put("LOW_PRESSURE_L", sp);

		sp = new SymbolPattern("Tropical Storm (Northern Hemisphere)");     //Sp25
		sp.addPath(new Coordinate[] {new Coordinate(2.0,5.0), new Coordinate(1.0,5.0),
									 new Coordinate(-1.0,4.0), new Coordinate(-2.0,2.0),
									 new Coordinate(-2.0,0.0), new Coordinate(-1.0,2.0),
									 new Coordinate(0.0,4.0), new Coordinate(2.0,5.0) }, true );
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,-5.0), new Coordinate(-1.0,-5.0),
				 					 new Coordinate(1.0,-4.0), new Coordinate(2.0,-2.0),
				 					 new Coordinate(2.0,0.0), new Coordinate(1.0,-2.0),
				 					 new Coordinate(0.0,-4.0), new Coordinate(-2.0,-5.0) }, true );
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,2.0), new Coordinate(-2.0,1.0),
				 					 new Coordinate(-2.0,0.0), new Coordinate(-1.0,-2.0),
				 					 new Coordinate(1.0,-2.0), new Coordinate(2.0,-1.0),
				 					 new Coordinate(2.0,0.0), new Coordinate(1.0,2.0),
				 					 new Coordinate(-1.0,2.0) }, false );
		patternMap.put("TROPICAL_STORM_NH", sp);

		sp = new SymbolPattern("Hurricane (Northern Hemisphere)");     //Sp26
		sp.addPath(new Coordinate[] {new Coordinate(2.0,5.0), new Coordinate(1.0,5.0),
				 					 new Coordinate(-1.0,4.0), new Coordinate(-2.0,2.0),
				 					 new Coordinate(-2.0,0.0), new Coordinate(-1.0,-2.0),
				 					 new Coordinate(1.0,-2.0), new Coordinate(0.0,-4.0),
				 					 new Coordinate(-2.0,-5.0), new Coordinate(-1.0,-5.0),
				 					 new Coordinate(1.0,-4.0), new Coordinate(2.0,-2.0),
				 					 new Coordinate(2.0,0.0), new Coordinate(1.0,2.0),
				 					 new Coordinate(-1.0,2.0), new Coordinate(0.0,4.0),
				 					 new Coordinate(2.0,5.0) }, true );
		patternMap.put("HURRICANE_NH", sp);

		sp = new SymbolPattern("Tropical Storm (Southern Hemisphere)");     //Sp27
		sp.addPath(new Coordinate[] {new Coordinate(2.0,-5.0), new Coordinate(0.0,-5.0),
									 new Coordinate(-2.0,-4.0), new Coordinate(-3.0,-2.0),
									 new Coordinate(-3.0,0.0), new Coordinate(-2.0,-2.0),
									 new Coordinate(-1.0,-4.0), new Coordinate(1.0,-5.0) }, true );
		sp.addPath(new Coordinate[] {new Coordinate(1.0,0.0), new Coordinate(0.0,2.0),
				 					 new Coordinate(-1.0,4.0), new Coordinate(-3.0,5.0),
				 					 new Coordinate(-2.0,5.0), new Coordinate(0.0,4.0),
				 					 new Coordinate(1.0,2.0), new Coordinate(1.0,0.0) }, true );
		sp.addPath(new Coordinate[] {new Coordinate(0.0,-2.0), new Coordinate(-2.0,-2.0),
				 					 new Coordinate(-3.0,-1.0), new Coordinate(-3.0,0.0),
				 					 new Coordinate(-2.0,2.0), new Coordinate(0.0,2.0),
				 					 new Coordinate(1.0,1.0), new Coordinate(0.0,-2.0),
				 					 new Coordinate(-2.0,-2.0) }, false );
		patternMap.put("TROPICAL_STORM_SH", sp);

		sp = new SymbolPattern("Hurricane (Southern Hemisphere)");     //Sp28
		sp.addPath(new Coordinate[] {new Coordinate(1.0,-5.0), new Coordinate(0.0,-5.0),
				 					 new Coordinate(-2.0,-4.0), new Coordinate(-3.0,-2.0),
				 					 new Coordinate(-3.0,0.0), new Coordinate(-2.0,2.0),
				 					 new Coordinate(0.0,2.0), new Coordinate(-1.0,4.0),
				 					 new Coordinate(-3.0,5.0), new Coordinate(-2.0,5.0),
				 					 new Coordinate(0.0,4.0), new Coordinate(1.0,2.0),
				 					 new Coordinate(1.0,0.0), new Coordinate(0.0,-2.0),
				 					 new Coordinate(-2.0,-2.0), new Coordinate(-1.0,-4.0),
				 					 new Coordinate(1.0,-5.0) }, true );
		patternMap.put("HURRICANE_SH", sp);

		sp = new SymbolPattern("Storm Center");     //Sp32
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,-5.0), new Coordinate(-5.0,5.0),
									 new Coordinate(5.0,5.0), new Coordinate(5.0,-5.0),
									 new Coordinate(-5.0,-5.0), new Coordinate(5.0,5.0) }, false );
		sp.addPath(new Coordinate[] {new Coordinate(5.0,-5.0), new Coordinate(-5.0,5.0) } );
		patternMap.put("STORM_CENTER", sp);

		sp = new SymbolPattern("Tropical Depression");     //Sp33
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,1.0), new Coordinate(-5.0,-1.0),
				 					 new Coordinate(-4.0,-3.0), new Coordinate(-3.0,-4.0),
				 					 new Coordinate(-1.0,-5.0), new Coordinate(1.0,-5.0),
				 					 new Coordinate(3.0,-4.0), new Coordinate(4.0,-3.0),
				 					 new Coordinate(5.0,-1.0), new Coordinate(5.0,1.0),
				 					 new Coordinate(4.0,3.0), new Coordinate(3.0,4.0),
				 					 new Coordinate(1.0,5.0), new Coordinate(-1.0,5.0),
				 					 new Coordinate(-3.0,4.0), new Coordinate(-4.0,3.0),
				 					 new Coordinate(-5.0,1.0) }, false );
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-3.0), new Coordinate(3.0,3.0) } );
		sp.addPath(new Coordinate[] {new Coordinate(3.0,-3.0), new Coordinate(-3.0,3.0) } );
		patternMap.put("TROPICAL_DEPRESSION", sp);

		sp = new SymbolPattern("Tropical Cyclone");     //Sp34
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,5.0), new Coordinate(5.0,5.0) } );
		sp.addPath(new Coordinate[] {new Coordinate(0.0,5.0), new Coordinate(0.0,-5.0) } );
		sp.addPath(new Coordinate[] {new Coordinate(5.0,-4.0), new Coordinate(4.0,-5.0),
									 new Coordinate(3.0,-5.0), new Coordinate(2.0,-4.0),
									 new Coordinate(2.0,-2.0), new Coordinate(3.0,-1.0),
									 new Coordinate(4.0,-1.0), new Coordinate(5.0,-2.0) }, false );
		patternMap.put("TROPICAL_CYCLONE", sp);

		sp = new SymbolPattern("Flame");     //Sp35
		sp.addPath(new Coordinate[] {new Coordinate(0.0,-5.0), new Coordinate(0.0,-4.0),
				 					 new Coordinate(1.0,-5.0), new Coordinate(2.0,-3.0),
				 					 new Coordinate(4.0,-2.0), new Coordinate(5.0,-1.0),
				 					 new Coordinate(5.0,0.0), new Coordinate(4.0,2.0),
				 					 new Coordinate(4.0,1.0), new Coordinate(2.0,-1.0),
				 					 new Coordinate(2.0,1.0), new Coordinate(1.0,5.0),
				 					 new Coordinate(0.0,3.0), new Coordinate(-1.0,4.0),
				 					 new Coordinate(-1.0,3.0), new Coordinate(-2.0,1.0),
				 					 new Coordinate(-2.0,0.0), new Coordinate(-1.0,-2.0),
				 					 new Coordinate(-1.0,-1.0), new Coordinate(1.0,0.0),
				 					 new Coordinate(0.0,-1.0), new Coordinate(0.0,-3.0),
				 					 new Coordinate(-3.0,-2.0), new Coordinate(-4.0,-1.0),
				 					 new Coordinate(-5.0,2.0), new Coordinate(-5.0,0.0),
				 					 new Coordinate(-4.0,-2.0), new Coordinate(-5.0,-1.0),
				 					 new Coordinate(-5.0,-2.0), new Coordinate(-4.0,-3.0),
				 					 new Coordinate(-5.0,-3.0), new Coordinate(0.0,-5.0) }, true );
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,2.0), new Coordinate(-3.0,0.0),
				 new Coordinate(-3.0,2.0), new Coordinate(-2.0,3.0),
				 new Coordinate(-2.0,4.0), new Coordinate(-3.0,3.0),
				 new Coordinate(-3.0,5.0), new Coordinate(-4.0,2.0),
				 new Coordinate(-4.0,0.0), new Coordinate(-3.0,-1.0),
					 new Coordinate(-4.0,2.0) }, true );
		patternMap.put("FLAME", sp);

		sp = new SymbolPattern("X Cross");     //Sp36
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,-5.0), new Coordinate(5.0,5.0) } );
		sp.addPath(new Coordinate[] {new Coordinate(5.0,-5.0), new Coordinate(-5.0,5.0) } );
		patternMap.put("X_CROSS", sp);

		sp = new SymbolPattern("LowX (outline)");     //Sp37
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,-2.0), new Coordinate(2.0,2.0) } );
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,2.0), new Coordinate(2.0,-2.0) } );
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,5.0), new Coordinate(-3.0,5.0),
									 new Coordinate(-3.0,-3.0), new Coordinate(2.0,-3.0),
									 new Coordinate(2.0,-5.0), new Coordinate(-5.0,-5.0),
									 new Coordinate(-5.0,5.0) }, false );
		patternMap.put("LOW_X_OUTLINE", sp);

		sp = new SymbolPattern("LowX (filled)");     //Sp38
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,-2.0), new Coordinate(2.0,2.0) } );
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,2.0), new Coordinate(2.0,-2.0) } );
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,5.0), new Coordinate(-3.0,5.0),
									 new Coordinate(-3.0,-3.0), new Coordinate(2.0,-3.0),
									 new Coordinate(2.0,-5.0), new Coordinate(-5.0,-5.0),
									 new Coordinate(-5.0,5.0) }, true );
		patternMap.put("LOW_X_FILLED", sp);

		sp = new SymbolPattern("Tropical Storm NH");     //Sp39
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,-2.0), new Coordinate(-3.0,-3.0),
									 new Coordinate(-1.0,-2.0), new Coordinate(0.0,0.0),
									 new Coordinate(3.0,0.0), new Coordinate(4.0,-1.0),
									 new Coordinate(5.0,-3.0) }, false );
		sp.addPath(new Coordinate[] {new Coordinate(0.0,0.0), new Coordinate(-1.0,1.0),
				 new Coordinate(-1.0,4.0), new Coordinate(0.0,5.0) }, false );
		patternMap.put("TROPICAL_STORM_NH_WPAC", sp);

		sp = new SymbolPattern("Tropical Storm SH");     //Sp40
		sp.addPath(new Coordinate[] {new Coordinate(5.0,-2.0), new Coordinate(3.0,-3.0),
									 new Coordinate(1.0,-2.0), new Coordinate(0.0,0.0),
									 new Coordinate(-3.0,0.0), new Coordinate(-4.0,-1.0),
									 new Coordinate(-5.0,-3.0) }, false );
		sp.addPath(new Coordinate[] {new Coordinate(0.0,0.0), new Coordinate(1.0,1.0),
				 new Coordinate(1.0,4.0), new Coordinate(0.0,5.0) }, false );
		patternMap.put("TROPICAL_STORM_SH_WPAC", sp);

		sp = new SymbolPattern("Nuclear Fallout");     //Sp41
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,1.0), new Coordinate(-3.0,5.0),
				 new Coordinate(-1.0,1.0), new Coordinate(-5.0,1.0) }, true );
		sp.addPath(new Coordinate[] {new Coordinate(1.0,1.0), new Coordinate(3.0,5.0),
				 new Coordinate(5.0,1.0), new Coordinate(1.0,1.0) }, true );
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,-5.0), new Coordinate(0.0,-1.0),
				 new Coordinate(2.0,-5.0), new Coordinate(-2.0,-5.0) }, true );
		sp.addDot(new Coordinate(0.0,0.0), 0.70);
		patternMap.put("NUCLEAR_FALLOUT", sp);

		sp = new SymbolPattern("Letter A filled");     //Sp43
		sp.addPath(new Coordinate[] {new Coordinate(5.0,-5.0), new Coordinate(3.0,-5.0),
				 new Coordinate(2.0,-2.0), new Coordinate(-2.0,-2.0),
				 new Coordinate(-1.0,0.0), new Coordinate(1.0,0.0),
				 new Coordinate(0.0,3.0), new Coordinate(-1.0,0.0),
				 new Coordinate(-2.0,-2.0), new Coordinate(-3.0,-5.0),
				 new Coordinate(-5.0,-5.0), new Coordinate(-1.0,5.0),
				 new Coordinate(1.0,5.0), new Coordinate(5.0,-5.0) }, true );
		patternMap.put("LETTER_A_FILLED", sp);

		sp = new SymbolPattern("Letter C");     //Sp44
		sp.addPath(new Coordinate[] {new Coordinate(5.0,-2.0), new Coordinate(4.0,-4.0),
				 new Coordinate(2.0,-5.0), new Coordinate(-2.0,-5.0),
				 new Coordinate(-4.0,-4.0), new Coordinate(-5.0,-2.0),
				 new Coordinate(-5.0,2.0), new Coordinate(-4.0,4.0),
				 new Coordinate(-2.0,5.0), new Coordinate(2.0,5.0),
				 new Coordinate(4.0,4.0), new Coordinate(5.0,2.0),
				 new Coordinate(3.0,2.0), new Coordinate(2.0,3.0),
				 new Coordinate(-2.0,3.0), new Coordinate(-3.0,2.0),
				 new Coordinate(-3.0,-2.0), new Coordinate(-2.0,-3.0),
				 new Coordinate(2.0,-3.0), new Coordinate(3.0,-2.0),
				 new Coordinate(5.0,-2.0) }, false );
		patternMap.put("LETTER_C", sp);

		sp = new SymbolPattern("Letter C filled");     //Sp45
		sp.addPath(new Coordinate[] {new Coordinate(5.0,-2.0), new Coordinate(4.0,-4.0),
				 new Coordinate(2.0,-5.0), new Coordinate(-2.0,-5.0),
				 new Coordinate(-4.0,-4.0), new Coordinate(-5.0,-2.0),
				 new Coordinate(-5.0,2.0), new Coordinate(-4.0,4.0),
				 new Coordinate(-2.0,5.0), new Coordinate(2.0,5.0),
				 new Coordinate(4.0,4.0), new Coordinate(5.0,2.0),
				 new Coordinate(3.0,2.0), new Coordinate(2.0,3.0),
				 new Coordinate(-2.0,3.0), new Coordinate(-3.0,2.0),
				 new Coordinate(-3.0,-2.0), new Coordinate(-2.0,-3.0),
				 new Coordinate(2.0,-3.0), new Coordinate(3.0,-2.0),
				 new Coordinate(5.0,-2.0) }, true );
		patternMap.put("LETTER_C_FILLED", sp);

		sp = new SymbolPattern("Letter X");     //Sp46
		sp.addPath(new Coordinate[] {new Coordinate(5.0,-5.0), new Coordinate(3.0,-5.0),
				 new Coordinate(0.0,-1.0), new Coordinate(-3.0,-5.0),
				 new Coordinate(-5.0,-5.0), new Coordinate(-1.0,0.0),
				 new Coordinate(-5.0,5.0), new Coordinate(-3.0,5.0),
				 new Coordinate(0.0,1.0), new Coordinate(3.0,5.0),
				 new Coordinate(5.0,5.0), new Coordinate(1.0,0.0),
				 new Coordinate(5.0,-5.0) }, false );
		patternMap.put("LETTER_X", sp);

		sp = new SymbolPattern("Letter X filled");     //Sp47
		sp.addPath(new Coordinate[] {new Coordinate(5.0,-5.0), new Coordinate(3.0,-5.0),
				 new Coordinate(0.0,-1.0), new Coordinate(-3.0,-5.0),
				 new Coordinate(-5.0,-5.0), new Coordinate(-1.0,0.0),
				 new Coordinate(-5.0,5.0), new Coordinate(-3.0,5.0),
				 new Coordinate(0.0,1.0), new Coordinate(3.0,5.0),
				 new Coordinate(5.0,5.0), new Coordinate(1.0,0.0),
				 new Coordinate(5.0,-5.0) }, true );
		patternMap.put("LETTER_X_FILLED", sp);

		sp = new SymbolPattern("Letter N");     //Sp48
		sp.addPath(new Coordinate[] {new Coordinate(5.0,-5.0), new Coordinate(3.0,-5.0),
				 new Coordinate(-3.0,2.0), new Coordinate(-3.0,-5.0),
				 new Coordinate(-5.0,-5.0), new Coordinate(-5.0,5.0),
				 new Coordinate(-3.0,5.0), new Coordinate(3.0,-2.0),
				 new Coordinate(3.0,5.0), new Coordinate(5.0,5.0),
				 new Coordinate(5.0,-5.0) }, false );
		patternMap.put("LETTER_N", sp);

		sp = new SymbolPattern("Letter N filled");     //Sp49
		sp.addPath(new Coordinate[] {new Coordinate(5.0,-5.0), new Coordinate(3.0,-5.0),
				 new Coordinate(-3.0,2.0), new Coordinate(-3.0,-5.0),
				 new Coordinate(-5.0,-5.0), new Coordinate(-5.0,5.0),
				 new Coordinate(-3.0,5.0), new Coordinate(3.0,-2.0),
				 new Coordinate(3.0,5.0), new Coordinate(5.0,5.0),
				 new Coordinate(5.0,-5.0) }, true );
		patternMap.put("LETTER_N_FILLED", sp);

		sp = new SymbolPattern("Thirty knot wind barb");     //Sp50
		sp.addPath(new Coordinate[] {new Coordinate(3.0,4.0), new Coordinate(5.0,4.0),
									 new Coordinate(5.0,0.0), new Coordinate(3.0,0.0),
									 new Coordinate(3.0,4.0) }, false );
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,4.0), new Coordinate(1.0,4.0),
				 new Coordinate(1.0,0.0), new Coordinate(-1.0,0.0) }, false );
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,-2.0), new Coordinate(-4.0,-5.0),
				 new Coordinate(4.0,-5.0) }, false );
		sp.addPath(new Coordinate[] {new Coordinate(1.0,2.0), new Coordinate(0.0,2.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,-2.0), new Coordinate(-2.0,-5.0) });
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,-2.0), new Coordinate(0.0,-5.0) });
		patternMap.put("30_KT_BARB", sp);

		/*
		 * Icing Symbols Section
		 */
		sp = new SymbolPattern("Light superstructure icing");     //Ice09
		sp.addPath(new Coordinate[] {new Coordinate(2.0,2.0), new Coordinate(2.0,0.0),
									 new Coordinate(1.0,-1.0), new Coordinate(-1.0,-1.0),
									 new Coordinate(-2.0,0.0), new Coordinate(-2.0,2.0),
									 new Coordinate(2.0,2.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(0.0,4.0), new Coordinate(0.0,-4.0) });
		patternMap.put("ICING_09", sp);

		sp = new SymbolPattern("Heavy superstructure icing");     //Ice10
		sp.addPath(new Coordinate[] {new Coordinate(3.0,2.0), new Coordinate(3.0,0.0),
									 new Coordinate(2.0,-1.0), new Coordinate(-2.0,-1.0),
									 new Coordinate(-3.0,0.0), new Coordinate(-3.0,2.0),
									 new Coordinate(3.0,2.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,4.0), new Coordinate(-1.0,-4.0) });
		sp.addPath(new Coordinate[] {new Coordinate(1.0,4.0), new Coordinate(1.0,-4.0) });
		patternMap.put("ICING_10", sp);

		/*
		 * Past Weather Symbols Section
		 */
		sp = new SymbolPattern("Thunderstorm");     //pastWx09
		sp.addPath(new Coordinate[] {new Coordinate(-2.0,1.0), new Coordinate(2.0,1.0),
									 new Coordinate(0.0,-1.0), new Coordinate(3.0,-4.0),
									 new Coordinate(3.0,-3.0), new Coordinate(3.0,-4.0),
									 new Coordinate(2.0,-4.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,-4.0), new Coordinate(-1.0,1.0) });
		patternMap.put("PAST_WX_09", sp);

		/*
		 * Sky Cover Symbols Section
		 */
		sp = new SymbolPattern("No cloud");     //sky00
//		sp.addPath(new Coordinate[] {new Coordinate(-5.0,1.0), new Coordinate(-5.0,-1.0), new Coordinate(-4.0,-3.0),
//									 new Coordinate(-3.0,-4.0), new Coordinate(-1.0,-5.0), new Coordinate(1.0,-5.0),
//									 new Coordinate(3.0,-4.0), new Coordinate(4.0,-3.0), new Coordinate(5.0,-1.0), 
//									 new Coordinate(5.0,1.0), new Coordinate(4.0,3.0), new Coordinate(3.0,4.0),
//									 new Coordinate(1.0,5.0), new Coordinate(-1.0,5.0), new Coordinate(-3.0,4.0), 
//									 new Coordinate(-4.0,3.0), new Coordinate(-5.0,1.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(-1.0,4.0), new Coordinate(-2.0,3.6), new Coordinate(-2.9,2.9),
				 new Coordinate(-3.6,2.0), new Coordinate(-4.0,1.0)  }, false );
		patternMap.put("SKY_COVER_00", sp);

		sp = new SymbolPattern("One-tenth or less");     //sky01
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(-1.0,4.0), new Coordinate(-2.0,3.6), new Coordinate(-2.9,2.9),
				 new Coordinate(-3.6,2.0), new Coordinate(-4.0,1.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(0.0,2.0), new Coordinate(0.0,-2.0) });
		patternMap.put("SKY_COVER_01", sp);

		sp = new SymbolPattern("Two-tenths to three-tenths");     //sky02
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(-1.0,4.0), new Coordinate(-2.0,3.6), new Coordinate(-2.9,2.9),
				 new Coordinate(-3.6,2.0), new Coordinate(-4.0,1.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(4.0,0.0), new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), 
				 new Coordinate(2.9,2.9), new Coordinate(2.0,3.6), new Coordinate(1.0,4.0), new Coordinate( 0.0,4.0), 
				 new Coordinate( 0.0,0.0), new Coordinate( 4.0,0.0)}, true);
		patternMap.put("SKY_COVER_02", sp);

		sp = new SymbolPattern("Four-tenths");     //sky03
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(-1.0,4.0), new Coordinate(-2.0,3.6), new Coordinate(-2.9,2.9),
				 new Coordinate(-3.6,2.0), new Coordinate(-4.0,1.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(4.0,0.0), new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), 
				 new Coordinate(2.9,2.9), new Coordinate(2.0,3.6), new Coordinate(1.0,4.0), new Coordinate( 0.0,4.0), 
				 new Coordinate( 0.0,0.0), new Coordinate( 4.0,0.0)}, true);
		sp.addPath(new Coordinate[] {new Coordinate(0.0,0.0), new Coordinate(0.0,-4.0) });
		patternMap.put("SKY_COVER_03", sp);

		sp = new SymbolPattern("Five-tenths");     //sky04
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(-1.0,4.0), new Coordinate(-2.0,3.6), new Coordinate(-2.9,2.9),
				 new Coordinate(-3.6,2.0), new Coordinate(-4.0,1.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(0.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(0.0,4.0), new Coordinate(0.0,-4.0)}, true );
		patternMap.put("SKY_COVER_04", sp);

		sp = new SymbolPattern("Six-tenths");     //sky05
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(-1.0,4.0), new Coordinate(-2.0,3.6), new Coordinate(-2.9,2.9),
				 new Coordinate(-3.6,2.0), new Coordinate(-4.0,1.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(0.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(0.0,4.0), new Coordinate(0.0,-4.0)}, true );
		sp.addPath(new Coordinate[] {new Coordinate(0.0,0.0), new Coordinate(-4.0,0.0) }); 
		patternMap.put("SKY_COVER_05", sp);

		sp = new SymbolPattern("Seven-tenths to eight-tenths");     //sky06
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(-1.0,4.0), new Coordinate(-2.0,3.6), new Coordinate(-2.9,2.9),
				 new Coordinate(-3.6,2.0), new Coordinate(-4.0,1.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,0.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(0.0,4.0), new Coordinate(0.0,0.0), new Coordinate(-4.0,0.0)}, true );
		patternMap.put("SKY_COVER_06", sp);        

		sp = new SymbolPattern("Nine-tenths or overcast with opening");     //sky07
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(-1.0,4.0), new Coordinate(-2.0,3.6), new Coordinate(-2.9,2.9),
				 new Coordinate(-3.6,2.0), new Coordinate(-4.0,1.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(0.5,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(0.5,4.0), new Coordinate(0.5,-4.0) }, true);
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(-0.5,-4.0),
				 new Coordinate(-0.5,4.0), new Coordinate(-1.0,4.0), new Coordinate(-2.0,3.6), new Coordinate(-2.9,2.9),
				 new Coordinate(-3.6,2.0), new Coordinate(-4.0,1.0) }, true);
		patternMap.put("SKY_COVER_07", sp);

		sp = new SymbolPattern("Completely overcast");     //sky08
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(-1.0,4.0), new Coordinate(-2.0,3.6), new Coordinate(-2.9,2.9),
				 new Coordinate(-3.6,2.0), new Coordinate(-4.0,1.0)  }, true );
		patternMap.put("SKY_COVER_08", sp);

		sp = new SymbolPattern("Sky obscured");     //sky09
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(-1.0,4.0), new Coordinate(-2.0,3.6), new Coordinate(-2.9,2.9),
				 new Coordinate(-3.6,2.0), new Coordinate(-4.0,1.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(-2.9,-2.9), new Coordinate(2.9,2.9) });
		sp.addPath(new Coordinate[] {new Coordinate(2.9,-2.9), new Coordinate(-2.9,2.9) });
		patternMap.put("SKY_COVER_09", sp);

		sp = new SymbolPattern("Missing cloud");     //sky10
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,1.0), new Coordinate(-4.0,-1.0), new Coordinate(-3.6,-2.0),
				 new Coordinate(-2.9,-2.9), new Coordinate(-2.0,-3.6), new Coordinate(-1.0,-4.0), new Coordinate(1.0,-4.0),
				 new Coordinate(2.0,-3.6), new Coordinate(2.9,-2.9), new Coordinate(3.6,-2.0), new Coordinate(4.0,-1.0), 
				 new Coordinate(4.0,1.0), new Coordinate(3.6,2.0), new Coordinate(2.9,2.9), new Coordinate(2.0,3.6),
				 new Coordinate(1.0,4.0), new Coordinate(-1.0,4.0), new Coordinate(-2.0,3.6), new Coordinate(-2.9,2.9),
				 new Coordinate(-3.6,2.0), new Coordinate(-4.0,1.0)  }, false );
		sp.addPath(new Coordinate[] {new Coordinate(-1.0,-3.0), new Coordinate(-1.0,3.0), new Coordinate(0.0,0.0),
									 new Coordinate(1.0,3.0), new Coordinate(1.0,-3.0) });
		patternMap.put("SKY_COVER_10", sp);
		
		/*
		 * Pressure Tendency Symbols Section
		 */
		sp = new SymbolPattern("Rising then falling");     //ptend00
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,-3.0), new Coordinate(1.0,3.0),
									 new Coordinate(4.0,0.0)  }, false );
		patternMap.put("PRESSURE_TENDENCY_00", sp);
		
		sp = new SymbolPattern("Rising then steady");     //ptend01
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,-3.0), new Coordinate(1.0,3.0),
									 new Coordinate( 4.0,3.0)  }, false );
		patternMap.put("PRESSURE_TENDENCY_01", sp);
		
		sp = new SymbolPattern("Rising steadily");     //ptend02
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,-3.0), new Coordinate(1.0,3.0) }, false );
		patternMap.put("PRESSURE_TENDENCY_02", sp);
		
		sp = new SymbolPattern("Falling or steady then rising");     //ptend03
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,0.0), new Coordinate( -1.0,-3.0),
									 new Coordinate( 5.0,3.0)  }, false );
		patternMap.put("PRESSURE_TENDENCY_03", sp);
		
		sp = new SymbolPattern("Steady");     //ptend04
		sp.addPath(new Coordinate[] {new Coordinate(-3.0,0.0), new Coordinate( 3.0,0.0),
									 new Coordinate(4.0,0.0)  }, false );
		patternMap.put("PRESSURE_TENDENCY_04", sp);
		
		sp = new SymbolPattern("Falling then rising");     //ptend05
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,3.0), new Coordinate( 1.0,-3.0),
									 new Coordinate(4.0,0.0)  }, false );
		patternMap.put("PRESSURE_TENDENCY_05", sp);
		
		sp = new SymbolPattern("Falling then steady");     //ptend06
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,3.0), new Coordinate( 1.0,-3.0),
									 new Coordinate( 4.0,-3.0)  }, false );
		patternMap.put("PRESSURE_TENDENCY_06", sp);
		
		sp = new SymbolPattern("Falling steadily");     //ptend07
		sp.addPath(new Coordinate[] {new Coordinate(-5.0,3.0), new Coordinate( 1.0,-3.0) }, false );
		patternMap.put("PRESSURE_TENDENCY_07", sp);
		
		sp = new SymbolPattern("Steady or rising then falling");     //ptend08
		sp.addPath(new Coordinate[] {new Coordinate(-4.0,0.0), new Coordinate( -1.0,3.0),
									 new Coordinate( 5.0,-3.0)  }, false );
		patternMap.put("PRESSURE_TENDENCY_08", sp);
	}
	
	/**
	 * Uses JAXB to marshal this list of SymoblPatterns to an XML file.
	 * @param filename output XML filename
	 */
	public void savePatternsToFile(String filename) {
		
		PrintWriter writer = null;
		
		/*
		 * Create a list of symbol patterns from the HashMap
		 */
		SymbolPatternList patternList = new SymbolPatternList(patternMap);
		
		try {
			// RTS utility: SerializationUtil.jaxbMarshalToXmlFile(lpm, filename);
			/*
			 * Set JAXB Marshaling context and properties
			 */
			JAXBContext context = JAXBContext.newInstance(SymbolPatternList.class, SymbolPatternMapEntry.class);
			Marshaller msh = context.createMarshaller();
			msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);

			/*
			 * create output file.
			 */
			File fileOut = new File(filename);
			writer = new PrintWriter(fileOut);

			/*
			 * Marshal list of symbol patterns to XML file
			 */
			msh.marshal(patternList, writer);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		finally {
			if ( writer != null ) writer.close();
		}
	}
	
	/**
	 * Uses JAXB to unmarshal a list of SymbolPatterns from an XML file,
	 * and load them into the internal HashMap. 
	 * @param filename input XML filename
	 */
	public void loadPatternsFromFile(String filename) {
		
		File fileIn = null;
		SymbolPatternList spl=null;
		
		try {
			//RTS utility: instance = (LinePatternManager) SerializationUtil.jaxbUnmarshalFromXmlFile(filename);
			JAXBContext context = JAXBContext.newInstance(SymbolPatternList.class);
			Unmarshaller msh = context.createUnmarshaller();
			
			/*
			 * unmarshal list of symbol patterns.
			 */
			fileIn = new File(filename);
			spl = (SymbolPatternList) msh.unmarshal(fileIn);

			// Add each LinePattern to hashMap
			for (SymbolPatternMapEntry entry : spl.getPatternList() ) {
			  patternMap.put(entry.getPatternId(), entry.getPattern());
			}
			
		}
		catch (Exception e) {
			e.printStackTrace();
		}

	}
	
}
