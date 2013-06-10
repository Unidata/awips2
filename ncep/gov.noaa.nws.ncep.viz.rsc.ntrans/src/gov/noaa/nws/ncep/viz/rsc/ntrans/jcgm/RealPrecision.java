/*
 * <copyright> Copyright 1997-2003 BBNT Solutions, LLC under sponsorship of the
 * Defense Advanced Research Projects Agency (DARPA).
 * Copyright 2009 Swiss AviationSoftware Ltd.
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the Cougaar Open Source License as published by DARPA on
 * the Cougaar Open Source Website (www.cougaar.org).
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm;

import java.io.DataInput;
import java.io.IOException;


/**
 * Class=1, Element=5
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class RealPrecision extends Command {
	enum Precision { FLOATING_32, FLOATING_64, FIXED_32, FIXED_64 }
	static private Precision precision;
	/**
	 * Flag to tell us whether a real precision command has already been
	 * processed or not
	 */
	static boolean realPrecisionProcessed;
	
	static {
		reset();
	}
    
    public RealPrecision(int ec, int eid, int l, DataInput in)
            throws IOException {

        super(ec, eid, l, in);
        int representation = makeEnum();
        int p2 = makeInt();
        int p3 = makeInt();
        if (representation == 0) {
        	if (p2 == 9 && p3 == 23) {
        		precision = Precision.FLOATING_32;
        	}
        	else if (p2 == 12 && p3 == 52) {
        		precision = Precision.FLOATING_64;
        	}
            else {
            	assert false : "unsupported combination";
            }
        }
        else if (representation == 1) {
        	if (p2 == 16 && p3 == 16) {
        		precision = Precision.FIXED_32;
        	}
        	else if (p2 == 32 && p3 == 32) {
        		precision = Precision.FIXED_64;
        	}
            else {
            	assert false : "unsupported combination";
            }
        }
        else {
        	assert false : "unsupported representation";
        }
        
        realPrecisionProcessed = true;
    }
    
    public static void reset() {
    	precision = Precision.FIXED_32;
    	realPrecisionProcessed = false;
	}

	static public Precision getPrecision() {
    	return RealPrecision.precision;
    }

	/**
	 * Returns a flag to tell us if we encountered a real precision command
	 * already or not
	 * 
	 * @return
	 */
	static public boolean hasRealPrecisionBeenProcessed() {
		return realPrecisionProcessed;
	}
	
    @Override
	public String toString() {
        String s = "RealPrecision " + String.valueOf(RealPrecision.precision);
        return s;
    }

}

/*
 * vim:encoding=utf8
 */
