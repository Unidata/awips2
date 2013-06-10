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

import java.io.*;

/**
 * Class=1, Element=19
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class ColourModel extends Command {
	enum Model { RGB, CIELAB, CIELUV, CMYK, RGB_RELATED }
	private static Model model;
	
	static {
		reset();
	}
	
    public ColourModel(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
        
        int index = makeIndex();
        switch (index) {
        case 1:
        	model = Model.RGB;
        	break;
        case 2:
        	model = Model.CIELAB;
        	break;
        case 3:
        	model = Model.CIELUV;
        	break;
        case 4:
        	model = Model.CMYK;
        	break;
        case 5:
        	model = Model.RGB_RELATED;
        	break;
        default:
        	unsupported("unsupported color mode "+index);
        	model = Model.RGB;
        }
        
        // make sure all the arguments were read
        assert (this.currentArg == this.args.length);
   }
    
    public static Model getModel() {
    	return model;
    }
    
    public static void reset() {
    	model = Model.RGB;
    }

    @Override
	public String toString() {
        return "ColourModel "+model;
    }
}

/*
 * vim:encoding=utf8
 */
