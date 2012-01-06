/****************************************************************************
 * NCSA HDF                                                                 *
 * National Comptational Science Alliance                                   *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * java-hdf5/COPYING file.                                                  *
 *                                                                          *
 ****************************************************************************/

package ncsa.hdf.hdf5lib.test;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class TestHDF5Library  
{
	public static String WRITE_FILE_ROOT = null;
	public static String READ_FILE_ROOT = null;
	public static String LOG = null;
	public static String HDF5_PATH = null;


	String dbgInfo;
	static boolean doLog = true;
	static DataOutputStream lp = null;

	boolean testALL;  // Test all the API functions.
	boolean testAPI;
	boolean testFileExceptions;  
	boolean testWrite;  
	boolean testString;  
	boolean testStringRead;  
	boolean testTypes;  
	boolean testInteger;  
	boolean testLong;  
	boolean testLongByteOrder;  
	boolean testLongByteOrderRead;  
	boolean testShort;  
	boolean testFloat;  
	boolean testFloatRead;  
	boolean testDouble;  
	boolean testDoubleRead;  
	boolean testByte;  
	boolean testCompound;  
	boolean testIter;  
	boolean testIntegerRead;  
	boolean testByteRead;  
	boolean testShortRead;  
	boolean testLongRead;  
	boolean testSelections;  
	boolean testException;  
	boolean testPalette;  
	boolean testRef;  
	boolean testOpaque;  

	boolean doframe = false;
	TestHDF5LibraryFrame testframe;

	public TestHDF5Library()
	{
		doframe = false;
		testframe = null;
		
		testALL  = false;
		testAPI  = false;
		testFileExceptions  = false;
		testWrite  = false;
		testString  = false;
		testStringRead  = false;
		testTypes  = false;
		testInteger  = false;
		testIntegerRead  = false;
		testByteRead  = false;
		testShortRead  = false;
		testLongRead  = false;
		testLong  = false;
		testLongByteOrder  = false;
		testLongByteOrderRead  = false;
		testShort  = false;
		testFloat  = false;
		testFloatRead  = false;
		testDouble  = false;
		testDoubleRead  = false;
		testByte  = false;
		testCompound  = false;
		testIter  = false;
		testSelections  = false;
		testException  = false;
		testPalette  = false;
		testRef  = false;

	}

public void doFrame() {
	testframe = new TestHDF5LibraryFrame();
	doframe = true;
}
	public static void main(String[] argv)
	{
		int argc = argv.length;
		TestHDF5Library test = new TestHDF5Library();
		int totalTests = 0;
		int passed = 0;

		for (int i=0; i<argc; i++)
		{
			String arg = argv[i].trim();
			if (arg.equalsIgnoreCase("-ReadDir"))
			{
				if (i < (argc - 1)) {
					i++;
					READ_FILE_ROOT = argv[i].trim();
					if (!READ_FILE_ROOT.endsWith("/")) {
						READ_FILE_ROOT += "/";
					}
				}
			} else if (arg.equalsIgnoreCase("-V"))
			{
				test.doFrame();
			} else if (arg.equalsIgnoreCase("-WriteDir"))
			{
				if (i < (argc - 1)) {
					i++;
					WRITE_FILE_ROOT = argv[i].trim();
					if (!WRITE_FILE_ROOT.endsWith("/")) {
						WRITE_FILE_ROOT += "/";
					}
				}
			} else if (arg.equalsIgnoreCase("-L"))
			{
				if (i < (argc - 1)) {
					i++;
					HDF5_PATH = argv[i].trim();
				}
			} else if (arg.equalsIgnoreCase("-LogFile"))
			{
				doLog = false;
				if (i < (argc - 1)) {
					i++;
					LOG = argv[i].trim();
					try {
					lp = new DataOutputStream(new FileOutputStream(LOG));
					} catch (Exception ex) {
						System.out.println("Can't open log: "+LOG);
						lp = null;
						doLog = false;
					}
					doLog = true;
				} else {
					doLog = false;
				}
			} else {
				test.setTestCase(argv[i]);
			}
		}
		if (READ_FILE_ROOT == null) {
			READ_FILE_ROOT = WRITE_FILE_ROOT;
		}

		if (!test.isTestCaseSet())
		{
			System.out.println("no test set?");System.out.flush();
         		test.help();
			test.dispose ();
			System.exit(1);
		}
		if (WRITE_FILE_ROOT == null)
		{
			System.out.println("no write dir set?");System.out.flush();
         		test.help();
			test.dispose ();
			System.exit(1);
		}

		if (test.testAPI || test.testALL) {
			TestAPI tt = new TestAPI();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testFileExceptions || test.testALL) {
			TestFileExceptions tt = new TestFileExceptions();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testWrite || test.testALL) {
			TestWrite tt = new TestWrite();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testString || test.testALL) {
			TestString tt = new TestString();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testStringRead || test.testALL) {
			TestStringRead tt = new TestStringRead();
			tt.setUp(READ_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testTypes || test.testALL) {
			TestTypes tt = new TestTypes();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testLong || test.testALL) {
			TestLong tt = new TestLong();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testInteger || test.testALL) {
			TestInteger tt = new TestInteger();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testShort || test.testALL) {
			TestShort tt = new TestShort();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testByte || test.testALL) {
			TestByte tt = new TestByte();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testFloat || test.testALL) {
			TestFloat tt = new TestFloat();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testFloatRead || test.testALL) {
			TestFloatRead tt = new TestFloatRead();
			tt.setUp(READ_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testDouble || test.testALL) {
			TestDouble tt = new TestDouble();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testDoubleRead || test.testALL) {
			TestDoubleRead tt = new TestDoubleRead();
			tt.setUp(READ_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testLongByteOrder || test.testALL) {
			TestLongByteOrder tt = new TestLongByteOrder();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testLongByteOrderRead || test.testALL) {
			TestLongByteOrderRead tt = new TestLongByteOrderRead();
			tt.setUp(READ_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testCompound || test.testALL) {
			TestCompound tt = new TestCompound();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testIter || test.testALL) {
			TestIter tt = new TestIter();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testIntegerRead || test.testALL) {
			TestIntegerRead tt = new TestIntegerRead();
			tt.setUp(READ_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testByteRead || test.testALL) {
			TestByteRead tt = new TestByteRead();
			tt.setUp(READ_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testShortRead || test.testALL) {
			TestShortRead tt = new TestShortRead();
			tt.setUp(READ_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testLongRead || test.testALL) {
			TestLongRead tt = new TestLongRead();
			tt.setUp(READ_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testSelections || test.testALL) {
			TestSelections tt = new TestSelections();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testException)  {  // not included in 'all'
			TestException tt = new TestException();
			tt.setUp(WRITE_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testPalette || test.testALL)  {
			TestPalette tt = new TestPalette();
			tt.setUp(READ_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testRef | test.testALL)  {
			TestRef tt = new TestRef();
			tt.setUp(READ_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}
		if (test.testOpaque | test.testALL)  {
			TestOpaque tt = new TestOpaque();
			tt.setUp(READ_FILE_ROOT, false);
			totalTests++;
			tt.runTest();
			boolean res = tt.testPassed();
			if (res) {
				passed++;
				test.appendMsg("\n"+tt.getTestName()+": PASS");
				test.logMsg("\n"+tt.getTestName()+": PASS");
			} else {
				test.appendMsg("\n"+tt.getVerboseResult());
				test.logMsg("\n"+tt.getVerboseResult());
			}
			tt.cleanUp(true);
			tt = null;
		}

		test.appendMsg("\n");
		test.appendMsg("\nAll tests completed: "+passed+" of "+totalTests+" passed");
		test.logMsg("\nAll tests completed: "+passed+" of "+totalTests+" passed");
		test.closeLog();
		test.flushMsg();
		test.close();
		System.exit(0);

	}

	public void close()
	{
		try {
/*
		h5.H5close();
*/
		H5.H5close();
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5close(): FAILED "+ex.getMessage();
			//msgBox.append("\n");
		}
	}

	public void logMsg(String msg)
	{
		if (doLog) {
			try {
				lp.writeUTF(msg);lp.flush();
			} catch (Exception ex) {
				System.out.println("write to log failed: ");
			}
		}
	}

	public void closeLog()
	{
		if (doLog) {
			try {
				lp.flush();
				lp.close();
				doLog = false;
			} catch (Exception ex) {
				System.out.println("write to log failed: ");
			}
		}
	}

	public void setTestCase(String testCase)
	{
		testCase = testCase.trim();

		if (testCase.equalsIgnoreCase("-ALL"))
			  testALL = true;
		else if (testCase.equalsIgnoreCase("-API"))
			  testAPI = true;
		else if (testCase.equalsIgnoreCase("-FileExceptions"))
			  testFileExceptions = true;
		else if (testCase.equalsIgnoreCase("-Write"))
			  testWrite = true;
		else if (testCase.equalsIgnoreCase("-String"))
			  testString = true;
		else if (testCase.equalsIgnoreCase("-StringRead"))
			  testStringRead = true;
		else if (testCase.equalsIgnoreCase("-Types"))
			  testTypes = true;
		else if (testCase.equalsIgnoreCase("-Integer"))
			  testInteger = true;
		else if (testCase.equalsIgnoreCase("-Long"))
			  testLong = true;
		else if (testCase.equalsIgnoreCase("-Short"))
			  testShort = true;
		else if (testCase.equalsIgnoreCase("-Byte"))
			  testByte = true;
		else if (testCase.equalsIgnoreCase("-Float"))
			  testFloat = true;
		else if (testCase.equalsIgnoreCase("-FloatRead"))
			  testFloatRead = true;
		else if (testCase.equalsIgnoreCase("-Double"))
			  testDouble = true;
		else if (testCase.equalsIgnoreCase("-DoubleRead"))
			  testDoubleRead = true;
		else if (testCase.equalsIgnoreCase("-LongByteOrder"))
			  testLongByteOrder = true;
		else if (testCase.equalsIgnoreCase("-Compound"))
			  testCompound = true;
		else if (testCase.equalsIgnoreCase("-Iter"))
			  testIter = true;
		else if (testCase.equalsIgnoreCase("-Selections"))
			  testSelections = true;
		else if (testCase.equalsIgnoreCase("-Exception"))
			  testException = true;
		else if (testCase.equalsIgnoreCase("-IntegerRead"))
			  testIntegerRead = true;
		else if (testCase.equalsIgnoreCase("-ByteRead"))
			  testByteRead = true;
		else if (testCase.equalsIgnoreCase("-ShortRead"))
			  testShortRead = true;
		else if (testCase.equalsIgnoreCase("-LongRead"))
			  testLongRead = true;
		else if (testCase.equalsIgnoreCase("-LongByteOrderRead"))
			  testLongByteOrderRead = true;
		else if (testCase.equalsIgnoreCase("-Palette"))
			  testPalette = true;
		else if (testCase.equalsIgnoreCase("-Ref"))
			  testRef = true;
		else if (testCase.equalsIgnoreCase("-Opaque"))
			  testOpaque = true;
	}

	public void help()
	{
		System.out.println("Usage: TestHDF5Library [-options]");
		System.out.println("	   -LogFile file	Save the output to 'file'");
		System.out.println("	   -WriteDir dir	create output files in 'dir'");
		System.out.println("	   -ReadDir dir	   read input files from 'dir'");
		System.out.println("	   -ALL	Test all the API functions");
		System.out.println("	   -API	Test the H5 API");
		System.out.println("	   -FileExceptions	Test Exceptions on file open/close ");
		System.out.println("	   -Write	Test Data Writes ");
		System.out.println("	   -String	Test Writes of Java Strings");
		System.out.println("	   -Types	Test Writes of Java Object Types");
		System.out.println("	   -Integer	Test Writes of Java Object Types");
		System.out.println("	   -Long	Test Writes of Java Object Types");
		System.out.println("	   -Short	Test Writes of Java Object Types");
		System.out.println("	   -Byte	Test Writes of Java Object Types");
		System.out.println("	   -Float	Test Writes of Java Object Types");
		System.out.println("	   -Double	Test Writes of Java Object Types");
		System.out.println("	   -DoubleRead	Test Reads of Java Object Types");
		System.out.println("	   -LongByteOrder	Test Writes of Java Object Types");
		System.out.println("	   -Compound	Test Writes of Java Object Types");
		System.out.println("	   -Iter	Test Writes of Java Object Types");
		System.out.println("	   -Selection	Test Selections");
		System.out.println("	   -Exception	Test Exception");
		System.out.println("	   -IntegerRead	Test Read of Java Object Types from another platform");
		System.out.println("	   -ByteRead	Test Read of Java Object Types from another platform");
		System.out.println("	   -ShortRead	Test Read of Java Object Types from another platform");
		System.out.println("	   -LongRead	Test Read of Java Object Types from another platform");
		System.out.println("	   -LongByteOrderRead	Test Reads of Java Object Types");
		System.out.println("	   -FloatRead	Test Reads of Java Object Types");
		System.out.println("	   -Opaque	Test Opaque Types");
	}

	public boolean isTestCaseSet()
	{
		return (
			testALL ||
			testAPI ||
			testWrite ||
			testFileExceptions ||
			testString ||
			testStringRead ||
			testTypes ||
			testInteger ||
			testShort ||
			testByte ||
			testFloat ||
			testFloatRead ||
			testDouble ||
			testDoubleRead ||
			testLong ||
			testLongByteOrder ||
			testLongByteOrderRead ||
			testCompound ||
			testIntegerRead ||
			testByteRead ||
			testShortRead ||
			testLongRead ||
			testIter ||
			testException ||
			testPalette ||
			testRef ||
			testOpaque ||
			testSelections );
	}

public void appendMsg(String msg) {
	if (doframe) testframe.msgBox.append(msg);
}
public void flushMsg() {
	if (doframe) testframe.msgBox.repaint();
}

public void dispose() {
	if (doframe) testframe.dispose();
}
}
