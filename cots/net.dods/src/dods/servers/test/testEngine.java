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

import dods.dap.*;
import dods.dap.Server.*;
/**
 * Used by the test server to reset the server output
 * for each new client request.
 *
 * @version $Revision: 1.7 $
 * @author ndp
 * @see BaseType
 */
public class testEngine {

    private static boolean _Debug = false;

    private ServerDDS  unconstrainedDDS;

    private boolean    tBool;        
    private byte       tByte;   
         
    private float      tFloat32;        
    private double     tFloat64;
            
    private short      tUint16;        
    private short      tInt16; 
         
    private int        tUint32;        
    private int        tInt32_1;        
    private int        tInt32_2; 
    
    private String     tURL; 
    private int        tStringCount; 
    
    
    /** Constructs a new <code>testFlags</code>. */
    public testEngine() {
        this((ServerDDS)null);
	}
	
    public testEngine(ServerDDS s) { 
    
        unconstrainedDDS = s;
    
        tBool            = false; 
        tByte            = 0;
	
        tFloat32         = (float) 0.0;
        tFloat64         = 0.0;

        tUint16          = 0;
        tInt16           = 0;

        tUint32          = 0;
        tInt32_1         = 0;
        tInt32_2         = 1;

        tURL             = "http://www.dods.org";
        tStringCount     = 0;
    }


    public boolean nextBool(){
        tBool = !tBool;
        return(tBool);
    }


    public byte nextByte(){
        return(tByte++);
    }


    public float nextFloat32(){
        float b = (float)(100 * Math.sin(tFloat32));
	tFloat32 += 0.01;
        return(b);
    }


    public double nextFloat64(){
	double b = (double) 1000*Math.cos(tFloat64);;
	tFloat64 += 0.01;
        return(b);
    }


    public short nextUint16(){
        short b = (short)(-16 * tUint16);
	tUint16++;
        return(b);
    }


    public short nextInt16(){
        short b = (short)(16 * tInt16);
	tInt16++;
        return(b);
    }


    public int nextUint32(){
        int b = tUint32++ * tUint32;
        return(b);
    }


    public int nextInt32(){
        int b;
	b = tInt32_1 + tInt32_2;
	tInt32_1 = tInt32_2;
	tInt32_2 = b;		
        return(b);
    }


    public String nextURL(){
        return(tURL);
    }


    public String nextString(){
        String b = "This is a data test string (pass " + tStringCount + ").";
	tStringCount++;
        return(b);
    }






    //**************************************************************************
    //
    // New ARRAY LOADER
    //
    //
    //
    //...........................................................................

    /** Loads test Arrays full of data */
    public void newLoadTestArray(String datasetName, test_SDArray ta) throws 
                                                    IOException,
						    EOFException {



        if(_Debug) System.out.println("---------------------------------------------------"); 
        if(_Debug) System.out.println("testEngine.newLoadTestArray(" + datasetName +"): "); 

        try {	    
            if(_Debug) System.out.println("Loading: " +
                                          ta.getName() + 
                                          " an SDArray of " + 
                                          ta.numDimensions() + 
                                          " dimension(s).");

            PrimitiveVector pv = ta.getPrimitiveVector();

            if(_Debug){
                Class cl = pv.getClass();		    	    
                System.out.println("PrimitiveVector is a: " + cl.getName());
            }


            pv.setLength(getLength(ta,0,true));
            if(_Debug)  System.out.println("Length: " + pv.getLength());

            newLoadArray(datasetName, ta);


        }
        catch (DODSException e){}
        if(_Debug) System.out.println("---------------------------------------------------"); 


    }


    private int getLength(test_SDArray ta, int dim, boolean constrained) throws InvalidParameterException{
    
    
        int sizeofOtherDims = 1;
    
        if (dim+1 < ta.numDimensions())
	        sizeofOtherDims = getLength(ta, dim+1, constrained);
    
        DArrayDimension dad = ta.getDimension(dim);
	
        int sizeofThisDim = 1;

	    if(constrained) {
                if(_Debug) System.out.print("Scanning Dimension " + dim + 
                                            "  start: " + dad.getStart() +
                                            "  stop: " + dad.getStop() +
                                            "  stride: " + dad.getStride());

                sizeofThisDim = 1 + (dad.getStop() - dad.getStart())/dad.getStride();
	    }
	    else {
	        sizeofThisDim = dad.getSize();
	    }

        int eCount = sizeofThisDim * sizeofOtherDims;

        if(_Debug) System.out.println("  length: " +sizeofThisDim); 

        return(eCount);
    }
    
      
    private int nuAI(int constrainedIndex, test_SDArray ta)throws InvalidParameterException {

   	int i, uI, k, dim;
	DArrayDimension dad;
	
	int cIndices[] = new int[ta.numDimensions()];
	int uIndices[] = new int[ta.numDimensions()];
	int cDimSteps[] = new int[ta.numDimensions()];
	int uDimSteps[] = new int[ta.numDimensions()];
	
	
	if(_Debug) System.out.println("ConstrainedIndex: "+constrainedIndex);
	
	
	dim = ta.numDimensions() - 1;
	cDimSteps[dim] = 1;
	uDimSteps[dim] = 1;
	for(dim=ta.numDimensions()- 2; dim>=0 ;dim--){
	    dad = ta.getDimension(dim+1);
	    cDimSteps[dim] = cDimSteps[dim+1] * dad.getSize(); 	    
	    uDimSteps[dim] = uDimSteps[dim+1] * ta.getCachedShape(dim+1); 
	}
	
	if(_Debug)  {
	    System.out.println("DimSteps: ");
	    for(dim=0; dim <ta.numDimensions(); dim++){
	        System.out.println("    cDimSteps["+dim+"]: "+cDimSteps[dim]);	    
	    }
	    System.out.println("");
	    for(dim=0; dim <ta.numDimensions(); dim++){
	        System.out.println("    uDimSteps["+dim+"]: "+uDimSteps[dim]);	    
	    }
	}
	
	
	if(_Debug) System.out.println("cIndices: ");
	
	k = 0;
	for(dim=0; dim<(ta.numDimensions()-1) ;dim++){
	    
	    dad = ta.getDimension(dim);

	    cIndices[dim]  = ( constrainedIndex - k) / cDimSteps[dim];
	   	    
	    if(_Debug) System.out.println("cIndices["+dim+"]: "+cIndices[dim]+"  k: "+k);
	    
	    k += cIndices[dim] * cDimSteps[dim];

	}
	
	cIndices[dim] =( constrainedIndex - k); 
	if(_Debug) System.out.println("cIndices["+dim+"]: "+cIndices[dim]+"  k: "+k);
	
	
	if(_Debug) System.out.print("uIndices: (");
	for(dim=0; dim<ta.numDimensions() ;dim++){   
	    dad = ta.getDimension(dim);
	    uIndices[dim] = dad.getStart() + cIndices[dim]*dad.getStride();	
	    if(_Debug) System.out.print(uIndices[dim]+", ");
	}
	if(_Debug) System.out.println(")");
	
	uI = 0;
	for(dim=0; dim<ta.numDimensions() ;dim++){
	    uI += uIndices[dim]*uDimSteps[dim];
	}
	
        return (uI);    
    
    
    }
    
    
    private void newLoadArray(String dataset, test_SDArray ta) throws InvalidParameterException, NoSuchVariableException, EOFException, IOException{

	
        PrimitiveVector pv = ta.getPrimitiveVector();
		
        if(_Debug) System.out.println("Loading Array... ");

        for(int j=0; j<pv.getLength() ; j++){
            if(_Debug) System.out.print("..\n");
	    
	    
	    
	    int i = nuAI(j,ta);
	    
            if(_Debug) System.out.println("ConstrainedIndex: "+j+"   UnconstrainedIndex: "+i);

            if(pv instanceof BaseTypePrimitiveVector){

                // get the archetype for this BaseType
                BaseType bt = ((BaseTypePrimitiveVector)pv).getTemplate();

                // Clone It.
                BaseType newBT = (BaseType)bt.clone();

                // Give it a new and appropriate name.
                newBT.setName(newBT.getName() + "["+j+"]");

                // Populate this Array member with the new object.
                ((BaseTypePrimitiveVector)pv).setValue(j,newBT);
		    
                // Get the clone back by use the get value method
                ServerMethods sm = (ServerMethods)((BaseTypePrimitiveVector)pv).getValue(j);

                // go and read some data into this newly minted bastard
                boolean MoreToRead = !(bt instanceof DSequence);
                while(MoreToRead){
                    MoreToRead = sm.read(dataset,this);
                }


            }
	    
            if(pv instanceof BooleanPrimitiveVector){
                if(i%2 !=0 )
                    ((BooleanPrimitiveVector)pv).setValue(j,true);
                else
                    ((BooleanPrimitiveVector)pv).setValue(j,false);
            }
            if(pv instanceof BytePrimitiveVector){
                ((BytePrimitiveVector)pv).setValue(j,(byte)i);				    
            }
            if(pv instanceof Float32PrimitiveVector){
                ((Float32PrimitiveVector)pv).setValue(j,(float)Math.sin(i/100.0));				    
            }
            if(pv instanceof Float64PrimitiveVector){
                ((Float64PrimitiveVector)pv).setValue(j,Math.cos(i/100.0));
            }
            if(pv instanceof Int16PrimitiveVector){
                ((Int16PrimitiveVector)pv).setValue(j,(short)(i*256));				    
            }
            if(pv instanceof UInt16PrimitiveVector){
                ((UInt16PrimitiveVector)pv).setValue(j,(short)(i*1024));				    
            }
            if(pv instanceof Int32PrimitiveVector){
                ((Int32PrimitiveVector)pv).setValue(j,i*2048);				    
            }
            if(pv instanceof UInt32PrimitiveVector){
                ((UInt32PrimitiveVector)pv).setValue(j,i*4096);				    
            }

        }
        if(_Debug) System.out.println("");		    
    }
    //**************************************************************************





    //**************************************************************************
    //
    // LIST LOADER
    //
    //
    //
    //...........................................................................
    
    /** Loads test Lists full of data */
    public void loadTestList(String datasetName, test_SDList tl) throws 
                                                    NoSuchVariableException,
                                                    IOException,
						    EOFException {
        if(_Debug) System.out.println("STARTING.....................test_SDList.read(" + datasetName +"): "); 

        if(_Debug) System.out.println("Loading: " + tl.getName() + " (an SDList)");
        PrimitiveVector pv = tl.getPrimitiveVector();

        if(_Debug){
            Class cl = pv.getClass();		    	    
            System.out.println("PrimitiveVector is a: " + cl.getName());
        }
		
        int ListLength = 3;
	pv.setLength(ListLength);
	if(_Debug)  System.out.println("List length arbitrarily set to: " + pv.getLength());

	if(_Debug)  System.out.println("Loading:");

	for(int j=0; j<ListLength ; j++){
	    if(_Debug) System.out.print(".");
		
	    if(pv instanceof BaseTypePrimitiveVector){

                // get the archetype for this BaseType
                BaseType bt = pv.getTemplate();

                // Clone It.
                BaseType newBT = (BaseType)bt.clone();
		
                // Give it a new and appropriate name.
                newBT.setName("testSDList_"+newBT.getName() + "["+j+"]");

                // Populate this list member the new object.
                ((BaseTypePrimitiveVector)pv).setValue(j,newBT);

                // Get the clone back by use the get value method
                newBT = ((BaseTypePrimitiveVector)pv).getValue(j);

                // go and read some data into this newly minted bastard
                boolean MoreToRead = !(bt instanceof DSequence);
		int pass = 0;
                while(MoreToRead){
		
                    if(_Debug) System.out.println("\nPass("+pass+"), reading data into "+newBT.getTypeName()+":");
		    if(_Debug) newBT.printDecl(System.out);
		    
                    MoreToRead = ((ServerMethods)newBT).read(datasetName, this);
		    
		    if(_Debug) System.out.println("Read returned: "+MoreToRead);
		    pass++;
                }			

    	    }
            if(pv instanceof BooleanPrimitiveVector){
                if(j%2 !=0 )
                    ((BooleanPrimitiveVector)pv).setValue(j,true);
                else
                    ((BooleanPrimitiveVector)pv).setValue(j,false);
            }
            if(pv instanceof BytePrimitiveVector){
                ((BytePrimitiveVector)pv).setValue(j,(byte)j);				    
            }
            if(pv instanceof Float32PrimitiveVector){
                ((Float32PrimitiveVector)pv).setValue(j,(float)Math.sin(j/100.0));				    
            }
            if(pv instanceof Float64PrimitiveVector){
                ((Float64PrimitiveVector)pv).setValue(j,Math.cos(j/100.0));
            }
            if(pv instanceof Int16PrimitiveVector){
                ((Int16PrimitiveVector)pv).setValue(j,(short)(j*255));				    
            }
            if(pv instanceof UInt16PrimitiveVector){
                ((UInt16PrimitiveVector)pv).setValue(j,(short)(j*1024));				    
            }
            if(pv instanceof Int32PrimitiveVector){
                ((Int32PrimitiveVector)pv).setValue(j,j*2048);				    
            }
            if(pv instanceof UInt32PrimitiveVector){
                ((UInt32PrimitiveVector)pv).setValue(j,j*4096);				    
            }

        }			
	if(_Debug) System.out.println("");		    
        if(_Debug) System.out.println("FINISHED....................test_SDList.read(" + datasetName +"): "); 
    }
    //**************************************************************************



    //**************************************************************************
    //
    // GRID LOADER
    //
    //
    //
    //...........................................................................
    
    /** Loads test Grids full of data */
    public void loadTestGrid(String datasetName, test_SDGrid tg) throws 
                                                    NoSuchVariableException,
                                                    IOException,
						    EOFException {
        SDArray da = (SDArray)tg.getVar(0);

        if(da.isProject())
            da.read(datasetName, this);

        for(int i=0; i<da.numDimensions() ;i++){

            try {
                DArrayDimension dad = da.getDimension(i);
            }
            catch (InvalidParameterException e) {
                throw new NoSuchVariableException(e.getMessage());
            }

            SDArray sam = (SDArray)tg.getVar(i+1);
            //System.out.println("The Map Vector Elements are: " + sam.getName());

            if(sam.isProject())
                sam.read(datasetName,this);

        }

    }
    //**************************************************************************





}
