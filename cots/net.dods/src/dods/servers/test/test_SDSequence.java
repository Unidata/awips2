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
import java.io.*;
import java.util.Vector;
import dods.dap.NoSuchVariableException;
import dods.dap.BaseType;
import dods.dap.Server.SDSequence;
import dods.dap.Server.ServerMethods;

/**
 * Holds a DODS Server <code>Sequence</code> value.
 *
 * @version $Revision: 1.3 $
 * @author ndp
 * @see BaseType
 */
public class test_SDSequence extends SDSequence {

    private static final boolean _Debug = false;

    private int sMaxLength = 5;
    private int sCount = 0;
    
  
    /** Constructs a new <code>test_SDSequence</code>. */
    public test_SDSequence() { 
        super(); 
	
    }

    /**
    * Constructs a new <code>test_SDSequence</code> with name <code>n</code>.
    * @param n the name of the variable.
    */
    public test_SDSequence(String n) { 
        super(n); 
    }

    // --------------- FileIO Interface

     /** Read a value from the named dataset for this variable. 
        @param datasetName String identifying the file or other data store
        from which to read a vaue for this variable.
        @param specialO This <code>Object</code> is a goody that is used by Server implementations
        to deliver important, and as yet unknown, stuff to the read method. If you
        don't need it, make it a <code>null</code>.
        @return <code>true</code> if more data remains to be read, otherwise
        <code>false</code>. This is an abtsract method that must be implemented
        as part of the installation/localization of a DODS server.
        @exception IOException
        @exception EOFException
    */
    public boolean read(String datasetName, Object specialO)
                            throws NoSuchVariableException, IOException, EOFException {

	boolean retVal, addRow = false;
	Vector rv = null;


	if(_Debug)System.out.println("\nReading row "+sCount+" of Sequence \"" + getName() + "\" from "+datasetName+":");

	rv = getRowVector();	    

	for(int i=0; i<rv.size() ;i++){
	
	    ServerMethods sm = (ServerMethods)rv.get(i);
	    
	    if(_Debug) System.out.println("Reading variable: "+((BaseType)sm).getTypeName()+", "+((BaseType)sm).getName());
	    
	    if(sm.isProject()){
		sm.read(datasetName, specialO);
                if(_Debug) ((BaseType)rv.get(i)).printVal(System.out,"   ");
	    }
	}


	sCount++;
	if (sCount < sMaxLength){
	    retVal = true;
	}
	else {
	    sCount = 0;
	    retVal = false;
	}

	setRead(true);	   

	if(_Debug) System.out.println("Read finished. Returning: "+retVal);
	if(_Debug && !retVal)  System.out.println("\n...........");
	return(retVal);
    }
}













