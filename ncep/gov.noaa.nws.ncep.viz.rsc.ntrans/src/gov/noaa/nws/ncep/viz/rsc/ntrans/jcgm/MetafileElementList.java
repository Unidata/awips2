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
 * Class=1, Element=11
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class MetafileElementList extends Command {
	String[] metaFileElements; 

    public MetafileElementList(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
        int nElements = makeInt();
        
        this.metaFileElements = new String[nElements];
        for (int i = 0; i < nElements; i++) {
        	int code1 = makeIndex();
        	int code2 = makeIndex();
        	if (code1 == -1) {
        		switch (code2) {
        		case 0:
        			this.metaFileElements[i] = "DRAWING SET";
        			break;
        		case 1:
        			this.metaFileElements[i] = "DRAWING PLUS CONTROL SET";
        			break;
        		case 2:
        			this.metaFileElements[i] = "VERSION 2 SET";
        			break;
        		case 3:
        			this.metaFileElements[i] = "EXTENDED PRIMITIVES SET";
        			break;
        		case 4:
        			this.metaFileElements[i] = "VERSION 2 GKSM SET";
        			break;
        		case 5:
        			this.metaFileElements[i] = "VERSION 3 SET";
        			break;
        		case 6:
        			this.metaFileElements[i] = "VERSION 4 SET";
        			break;
        		default:
        			unsupported("unsupported meta file elements set "+code2);
        		}
        	}
        	else {
        		// note: here, we can easily determine if a class/element is implemented or not
        		StringBuilder sb = new StringBuilder();
        		sb.append(" (").append(code1).append(",").append(code2).append(")");
        		this.metaFileElements[i] = sb.toString();
        	}
        }
        
        // make sure all the arguments were read
        assert (this.currentArg == this.args.length);
    }

    @Override
	public String toString() {
        String s = "MetafileElementList ";
        for (String element: this.metaFileElements) {
        	s += element + " ";
        }
        return s;
    }
}

/*
 * vim:encoding=utf8
 */
