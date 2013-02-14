/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1999, COAS, Oregon State University  
// ALL RIGHTS RESERVED.   U.S. Government Sponsorship acknowledged. 
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: Nathan Potter (ndp@oce.orst.edu)
//
//                        College of Oceanic and Atmospheric Scieneces
//                        Oregon State University
//                        104 Ocean. Admin. Bldg.
//                        Corvallis, OR 97331-5503
//         
/////////////////////////////////////////////////////////////////////////////

package dods.servers.test;

import dods.util.gui.*;

/**
 * Test routine for the SD classes
 *
 * @version $Revision: 1.2 $
 * @author ndp
 */

import java.io.*;
import java.util.Enumeration;
import gnu.getopt.Getopt;
import dods.dap.BaseType;
import dods.dap.DODSException;
import dods.dap.Server.*;

public class SDTest {


    public static boolean Debug = false;

    public static String DDSFile, ConstraintExpression;


    // Constructor
    public SDTest() {
    }


    //***************************************************************
    // Dump the Server DDS contents to stSystem.out.
    public static void print_SDDS(ServerDDS sdds,boolean constrained){
    
        System.out.println("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv");
        System.out.println("ServerDDS:");
        Enumeration e = sdds.getVariables();

        while( e.hasMoreElements() ) {
	    Object o        = e.nextElement();
	    ServerMethods s = (ServerMethods)o;
	    BaseType bt     = (BaseType)o;

	    System.out.println(bt.getTypeName()+" "+bt.getName()+":");
	    System.out.println("Constrained DDS:");
		
	    bt.printDecl(System.out, "    ", true, constrained);


	    System.out.println("Declaration and Value:");
/*

            if(s.isRead()){
	    
		try { 
		    bt.printVal(System.out, "    ",true);
		}
		    catch(NullPointerException except){
		    System.out.println(" Instance not Allocated.");
		}
	    }
	    else {
		System.out.println(" Item not yet initialized.");
	    }
	    
*/
	    
		
            if( s.isRead()){	
                    bt.printVal(System.out, "    ",true);
            }
            else {
                    bt.printDecl(System.out, "    ");
            }
		
    
	    
	    
			
	    System.out.print  (" isProj: " + s.isProject());
	    System.out.print  ("    isRead: " + s.isRead());
	    System.out.println("    isSynth: " + s.isSynthesized());
	    if(e.hasMoreElements() )
	    	System.out.println("- - - - - - - - - - - - - - - - - -");
        }
        System.out.println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");

    }
    //***************************************************************



    //***************************************************************
    public static void parse_options(String[] args){

		
        Getopt g = new Getopt("SDTest", args, "f:c:");

        int c;
        String arg;
        while ((c = g.getopt()) != -1) {
            switch(c){
                case 'f':
                    arg = g.getOptarg();
                    if(Debug) System.out.print("DDS File: " +
                                     ((arg != null) ? arg : "null") + "\n");
				    DDSFile = arg;
                    break;
                case 'c':
                    arg = g.getOptarg();
                    if(Debug) System.out.print("Constraint Expression: \"" +
                                     ((arg != null) ? arg : "null") + "\"\n");
				    
				    ConstraintExpression = arg;
                    break;
                //
                case '?':
                    break; // getopt() already printed an error
                //
                default:
                    if(Debug) System.out.print("getopt() returned " + c + "\n");
            }
        }
    }
    //***************************************************************
    
    

    public static void main(String[] args) throws Exception{
    
	SDTest sdt = new SDTest();

	try {

	    System.out.println("-------------------------------------------");

	    System.out.println("Debugging Display: " + (Debug?"ON":"OFF"));
	    parse_options(args);	

	    System.out.println("...........................................");

	    File fin = new File(DDSFile);
	    FileInputStream fp_in = new FileInputStream(fin);
	    DataInputStream dds_source = new DataInputStream(fp_in);

	    test_ServerFactory sfactory = new test_ServerFactory();
	    ServerDDS myDDS = new ServerDDS("bogus",sfactory);

	    if(Debug) System.out.println("Parsing DDS...");
	    myDDS.parse(dds_source);

	    if(Debug) System.out.println("Printing DDS...");
	    myDDS.print(System.out);

	    print_SDDS(myDDS,false);

	    if(Debug) System.out.println("Constructing CEEvaluator...");
	    CEEvaluator ce = new CEEvaluator(myDDS);

	    File fout = new File("a.out");
	    FileOutputStream fp_out = new FileOutputStream(fout);
	    DataOutputStream sink = new DataOutputStream(fp_out);

	    if(Debug)System.out.println("Parsing Constraint Expression: "+ConstraintExpression);
	    ce.parseConstraint(ConstraintExpression);

	    if(Debug) System.out.println("Attempting to send data...");
	    ce.send(myDDS.getName(),sink,null);


	    print_SDDS(myDDS,true);
	    myDDS.printConstrained(System.out);

	    System.out.println("-------------------------------------------");


	}
	catch (DODSException e) {
	    System.out.println("\n\nERROR of Type: " + e.getClass().getName()+"\n");
	    System.out.println("Message:\n"+ e.getMessage()+"\n");
	    System.out.println("Stack Trace: ");
	    e.printStackTrace(System.out);
	    System.out.println("\n\n");
	}

	System.exit(0);
    }


}

