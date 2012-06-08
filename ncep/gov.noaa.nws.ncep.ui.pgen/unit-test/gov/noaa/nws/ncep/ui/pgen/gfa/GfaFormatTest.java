package gov.noaa.nws.ncep.ui.pgen.gfa;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;

import java.io.FileNotFoundException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

//import org.apache.log4j.Logger;
import org.dom4j.Document;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

public class GfaFormatTest {

	/** Logger */
//	private final static Logger log = Logger.getLogger(GfaFormatTest.class);

	public static final String BASE_DIR = "../../eclipse/";
	public static final int CYCLE_DAY = 20;
	public static final int CYCLE_HOUR = 14;
	public static final String HTTP_SERVER = "http://localhost:9581/services";

	public static final String PACKAGE = "unit-test/"
			+ GfaFormatTest.class.getPackage().getName().replace(".", "/") + "/xml/";
	public static String rulesXml = "rules.xml";
	
	private static Document doc;

	@Before
	public void setUp() throws Exception {

		configure();
		getDocument();
	}

	@After
	public void tearDown() throws Exception {
	}

	@Ignore
	public void getDocumentTest() {
		// reads the rules file
		assertNotNull(doc);
	}

	private void createSmearsTest(String fileName, GfaWording expectedWording, int adj) throws FileNotFoundException, SerializationException {
		List<AbstractDrawableComponent> drawables = read(PACKAGE + fileName);
		// test if all gfa and size
		ArrayList<Gfa> gfaList = new ArrayList<Gfa>();
		for (AbstractDrawableComponent adc : drawables) {
			if (adc instanceof Gfa)
				gfaList.add((Gfa) adc);
		}
		assertFalse(gfaList.isEmpty());

		int sizeBefore = gfaList.size();

		GfaFormat gf = new GfaFormat();

		// actual test
		gf.createSmears(drawables);

		int sizeAfter = drawables.size();
		assertTrue(sizeAfter > sizeBefore);

		Gfa gfa = (Gfa) drawables.get(sizeBefore + adj);
		GfaWording e = expectedWording; // expected
		GfaWording r = gfa.getAttribute("WORDING", GfaWording.class); // result 
		assertEquals("failed fromCondsDvlpg in " + fileName, e.fromCondsDvlpg, r.fromCondsDvlpg);
		assertEquals("failed fromCondsEndg in " + fileName, e.fromCondsEndg, r.fromCondsEndg);
		assertEquals("failed genOlk in " + fileName, e.genOlk, r.genOlk);
		assertEquals("failed condsContg in " + fileName, e.condsContg, r.condsContg);
		assertEquals("failed otlkCondsDvlpg in " + fileName, e.otlkCondsDvlpg, r.otlkCondsDvlpg);
		assertEquals("failed otlkCondsEndg in " + fileName, e.otlkCondsEndg, r.otlkCondsEndg);
		assertEquals("failed in " + fileName, e, r);
	}
	
	@Test
	public void runRules() {
		String xPath = "/rules/rule";
		List<Node> nodes = selectNodes(xPath);
		StringBuilder sb = new StringBuilder(500);
		int errors = 0;
		for (Node n : nodes) {
			String[] fileNames = n.valueOf("@filenames").split(",");
			
			for(String fileName: fileNames){
//				if(!fileName.startsWith("gfa01.xml")) continue;
				
				String fromCondsDvlpg = n.valueOf("@fromCondsDvlpg");
				String fromCondsEndg = n.valueOf("@fromCondsEndg");
				String genOlk = n.valueOf("@genOlk");
				String condsContg = n.valueOf("@condsContg");
				String otlkCondsDvlpg = n.valueOf("@otlkCondsDvlpg");
				String otlkCondsEndg = n.valueOf("@otlkCondsEndg");
				String orderInQueue = n.valueOf("@orderInQueue");
				int plus = 0;
				if (!orderInQueue.isEmpty()) {
					plus = Integer.parseInt(orderInQueue);
				}
				
				GfaWording expected = new GfaWording();
				expected.fromCondsDvlpg = fromCondsDvlpg;
				expected.fromCondsEndg = fromCondsEndg;
				expected.genOlk = genOlk;
				expected.condsContg = condsContg;
				expected.otlkCondsDvlpg = otlkCondsDvlpg;
				expected.otlkCondsEndg = otlkCondsEndg;
				sb.append("filename=\"" + fileName + "\"\nExpected:\n");
				sb.append(expected.toString());
//				log.info(sb.toString());
				sb.setLength(0);
				try {
					createSmearsTest(fileName, expected, plus);
				} catch (Exception e) {
//					log.error("\ncreateSmearsTest failed: " + fileName, e);
					e.printStackTrace();
					errors++;
				}
			}
		}
		
		assertEquals("Number of failed files ", 0, errors);
		
//		log.info(sb.toString());
	}

	private List<AbstractDrawableComponent> read(String file) throws FileNotFoundException, SerializationException {
		Products products = (Products) Util.read(file);
		List<Product> productList = ProductConverter.convert(products);
		assertEquals(1, productList.size());
		List<Layer> layerList = productList.get(0).getLayers();
		assertEquals(1, layerList.size());
		List<AbstractDrawableComponent> drawables = layerList.get(0).getDrawables();
		return drawables;
	}

	/**
	 * Sets base directory and cycle.
	 * 
	 * @throws NoSuchFieldException
	 * @throws IllegalAccessException
	 */
	public static void configure() throws NoSuchFieldException, IllegalAccessException {

		long time = System.currentTimeMillis();

		LocalizationManager.setBaseDir(GfaFormatTest.BASE_DIR);
		// use reflection to update private fields
		Field field = PgenCycleTool.class.getDeclaredField("cycleDay");
		field.setAccessible(true);
		field.set(null, GfaFormatTest.CYCLE_DAY);
		field = PgenCycleTool.class.getDeclaredField("cycleHour");
		field.setAccessible(true);
		field.set(null, GfaFormatTest.CYCLE_HOUR);

		//?NcDirectDbQuery.setHttpServer(HTTP_SERVER);

//		log.debug("configure() " + (System.currentTimeMillis() - time) + " ms");

		if(!PreloadGfaDataThread.loaded) {
			// preload the classes to reduce the first GFA format time 
			new PreloadGfaDataThread().run();
		}
	}
	
	static int count = 0;
	private void getDocument() {
		if (doc != null) return;
		try {
			SAXReader reader = new SAXReader();
			doc = reader.read(PACKAGE + rulesXml);
		} catch (Exception e) {
//			log.error(e);
			e.printStackTrace();
		}
	}
	
	@SuppressWarnings("unchecked")
	private List<Node> selectNodes(String xPath) {
		return doc.selectNodes(xPath);
	}
}
