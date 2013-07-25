package gov.noaa.nws.ncep.gempak.parameters.title;

import gov.noaa.nws.ncep.gempak.parameters.title.TITLE;
import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;
import org.eclipse.swt.graphics.RGB;
import org.junit.Test;
import static org.junit.Assert.*;

//Title format:title color / title line location / title string | short title

public class TITLETest {

	private static int testCaseNumber=1;
	
	@Test
	public void testTitleStringWithoutSlashesParser () {

		System.out.println("------------------Test-case "+ testCaseNumber +" Title string without slash----------------");

		TITLE title = new TITLE("500MB Height");

		System.out.println ("Input Title String: " + "500MB Height");
        if (title.getTitleString()!= null ) {
        	RGB rgb = GempakColor.convertToRGB ( 1 );
        	assertEquals (title.getTitleColor(),rgb);
        	assertEquals (title.getTitleLineLoaction(),0);
        	assertEquals (title.getTitleString(),"500MB Height");
        	System.out.println ( "Title Information:" );
    		System.out.println ( "    Title Color: " + title.getTitleColor().red + " " + title.getTitleColor().green + " " + title.getTitleColor().blue );
    		System.out.println ( "    Title Line Location: " + title.getTitleLineLoaction() );
    		System.out.println ( "    Title String: " + title.getTitleString());
        }
        testCaseNumber ++;
	}

	@Test
	public void testTitleStringWithOneSlashesParser () {

		System.out.println("------------------Test-case "+ testCaseNumber +" Title string with one slash----------------");

		TITLE title = new TITLE("5/- GOES 6km SST (deg C)");

		System.out.println ("Input Title String: " + "5/~ GOES 6km SST (deg C)");
        if (title.getTitleString()!= null ) {
        	RGB rgb = GempakColor.convertToRGB ( 5 );
        	assertEquals (title.getTitleColor(),rgb);
        	assertEquals (title.getTitleLineLoaction(),0);
        	assertEquals (title.getTitleString(),"~ @ _$");
        	System.out.println ( "Title Information:" );
    		System.out.println ( "    Title Color: " + title.getTitleColor().red + " " + title.getTitleColor().green + " " + title.getTitleColor().blue );
    		System.out.println ( "    Title Line Location: " + title.getTitleLineLoaction() );
    		System.out.println ( "    Title String: " + title.getTitleString());
        }
        testCaseNumber ++;
	}
	
	@Test
	public void testTitleStringWithoutColorParser () {

		System.out.println("------------------Test-case "+ testCaseNumber +" Title string with two slashes. No title color value----------------");

		TITLE title = new TITLE(" /-5/- GOES 6km SST (deg C)");

		System.out.println ("Input Title String: " + " /-5/~ GOES 6km SST (deg C)");
		
        if (title.getTitleString() == null ) {
        	assertEquals (title.getTitleString(),null);
    		System.out.println ( "    Title String: " + title.getTitleString());
        }
        testCaseNumber ++;
	}
	
	@Test
	public void testTitleStringWithoutLocationParser () {

		System.out.println("------------------Test-case "+ testCaseNumber +" Title string with two slashes. No title line location value----------------");

		TITLE title = new TITLE("2/ /~ GOES 6km SST (deg C)");

		System.out.println ("Input Title String: " + "2/ /~ GOES 6km SST (deg C)");
        if (title.getTitleString()!= null ) {
        	RGB rgb = GempakColor.convertToRGB ( 2 );
        	assertEquals (title.getTitleColor(),rgb);
        	assertEquals (title.getTitleLineLoaction(),0);
        	assertEquals (title.getTitleString(),"~ GOES 6km SST (deg C)");
        	System.out.println ( "Title Information:" );
    		System.out.println ( "    Title Color: " + title.getTitleColor().red + " " + title.getTitleColor().green + " " + title.getTitleColor().blue );
    		System.out.println ( "    Title Line Location: " + title.getTitleLineLoaction() );
    		System.out.println ( "    Title String: " + title.getTitleString());
        }
        testCaseNumber ++;
	}
	
	@Test
	public void testTitleStringWithoutTitleParser () {

		System.out.println("------------------Test-case "+ testCaseNumber +" Title string with two slashes. No title string value----------------");

		TITLE title = new TITLE("2/-5/ ");

		System.out.println ("Input Title String: " + "2/-5/ ");
        if (title.getTitleString()!= null ) {
        	RGB rgb = GempakColor.convertToRGB ( 2 );
        	assertEquals (title.getTitleColor(),rgb);
        	assertEquals (title.getTitleLineLoaction(),-5);
        	assertEquals (title.getTitleString(),"~ @ _$");
        	System.out.println ( "Title Information:" );
    		System.out.println ( "    Title Color: " + title.getTitleColor().red + " " + title.getTitleColor().green + " " + title.getTitleColor().blue );
    		System.out.println ( "    Title Line Location: " + title.getTitleLineLoaction() );
    		System.out.println ( "    Title String: " + title.getTitleString());
        }
        testCaseNumber ++;
	}
	
	@Test
	public void testTitleStringTowSlashesWithTitleParser () {

		System.out.println("------------------Test-case "+ testCaseNumber +" Title string with two slashes. No title color/line location value----------------");

		TITLE title = new TITLE(" / /- GOES 6km SST (deg C)");

		System.out.println ("Input Title String: " + " / /~ GOES 6km SST (deg C)");
        if (title.getTitleString()== null ) {
        	assertEquals (title.getTitleString(),null);
    		System.out.println ( "    Title String: " + title.getTitleString());
        }
        testCaseNumber ++;
	}
	
	@Test
	public void testTitleStringTowSlashesWithLocationParser () {

		System.out.println("------------------Test-case "+ testCaseNumber +" Title string with two slashes. No title color/string value----------------");

		TITLE title = new TITLE(" /-5/ ");

		System.out.println ("Input Title String: " + " /-5/ ");
        if (title.getTitleString()== null ) {
        	assertEquals (title.getTitleString(),null);
    		System.out.println ( "    Title String: " + title.getTitleString());
        }
        testCaseNumber ++;
	}
	
	@Test
	public void testTitleStringOnlyWithColorParser () {

		System.out.println("------------------Test-case "+ testCaseNumber +" Title string with two slashes. No title line location/string value----------------");

		TITLE title = new TITLE("2/ / ");

		System.out.println ("Input Title String: " + "2/ / ");
        if (title.getTitleString()!= null ) {
        	RGB rgb = GempakColor.convertToRGB ( 2 );
        	assertEquals (title.getTitleColor(),rgb);
        	assertEquals (title.getTitleLineLoaction(),0);
        	assertEquals (title.getTitleString(),"~ @ _$");
        	System.out.println ( "Title Information:" );
    		System.out.println ( "    Title Color: " + title.getTitleColor().red + " " + title.getTitleColor().green + " " + title.getTitleColor().blue );
    		System.out.println ( "    Title Line Location: " + title.getTitleLineLoaction() );
    		System.out.println ( "    Title String: " + title.getTitleString());
        }
        testCaseNumber ++;
	}
	
	@Test
	public void testTitleStringTowSlashesWithoutAnyValuesParser () {

		System.out.println("------------------Test-case "+ testCaseNumber +" Title string with two slashes. No value for all three parameters----------------");

		TITLE title = new TITLE(" / / ");

		System.out.println ("Input Title String: " + " / / ");
        if (title.getTitleString()== null ) {
        	assertEquals (title.getTitleString(),null);
    		System.out.println ( "    Title String: " + title.getTitleString());
        }
        testCaseNumber ++;
	}
	
	@Test
	public void testTitleStringWithAllValuesParser () {

		System.out.println("------------------Test-case "+ testCaseNumber +" Title string with two slashes----------------");

		TITLE title = new TITLE("2/-5/~ GOES 6km SST (deg C)");

		System.out.println ("Input Title String: " + "2/-5/~ GOES 6km SST (deg C)");
        if (title.getTitleString()!= null ) {
        	RGB rgb = GempakColor.convertToRGB ( 2 );
        	assertEquals (title.getTitleColor(),rgb);
        	assertEquals (title.getTitleLineLoaction(),-5);
        	assertEquals (title.getTitleString(),"~ GOES 6km SST (deg C)");
        	System.out.println ( "Title Information:" );
    		System.out.println ( "    Title Color: " + title.getTitleColor().red + " " + title.getTitleColor().green + " " + title.getTitleColor().blue );
    		System.out.println ( "    Title Line Location: " + title.getTitleLineLoaction() );
    		System.out.println ( "    Title String: " + title.getTitleString());
        }
        testCaseNumber ++;
	}
	
	@Test
	public void testEmptyTitleStringParser () {

		System.out.println("------------------Test-case "+ testCaseNumber +" Empty Title String----------------");

		TITLE title = new TITLE("");

		System.out.println ("Input Title String: " + "");
        if (title.getTitleString()== null ) {
        	assertEquals (title.getTitleString(),null);
    		System.out.println ( "    Title String: " + title.getTitleString());
        }
        testCaseNumber ++;
	}
}
