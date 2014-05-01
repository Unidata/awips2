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
 * Class=5, Element=22
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class InteriorStyle extends Command {
	protected enum Style {
		HOLLOW,
		SOLID,
		PATTERN,
		HATCH,
		EMPTY,
		GEOMETRIC_PATTERN,
		INTERPOLATED
	}
	
	protected Style style;

    public InteriorStyle(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
        switch (makeEnum()) {
        case 0:
        	this.style = Style.HOLLOW;
        	break;
        case 1:
        	this.style = Style.SOLID;
        	break;
        case 2:
        	this.style = Style.PATTERN;
        	break;
        case 3:
        	this.style = Style.HATCH;
        	break;
        case 4:
        	this.style = Style.EMPTY;
        	break;
        case 5:
        	this.style = Style.GEOMETRIC_PATTERN;
        	break;
        case 6:
        	this.style = Style.INTERPOLATED;
        	break;
        default:
        	this.style = Style.HOLLOW;
        }
        
        if (!Style.HOLLOW.equals(this.style) || !Style.SOLID.equals(this.style) ||
			!Style.HATCH.equals(this.style) || !Style.EMPTY.equals(this.style)) {
			unimplemented(this.style.toString());
		}
    }

    @Override
	public String toString() {
    	StringBuilder sb = new StringBuilder();
    	sb.append("InteriorStyle ").append(this.style);
    	return sb.toString();
    }

	@Override
	public void paint(CGMDisplay d) {
		d.setInteriorStyle(this.style);
	}
}

/*
 * vim:encoding=utf8
 */
