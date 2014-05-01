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
 * Class=1, Element=12
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class MetafileDefaultsReplacement extends Command {
	private Command embeddedCommand;

	public MetafileDefaultsReplacement(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
        
        int k = makeUInt(16);
        
        // the element class
        int elementClass = k >> 12;
        int elementId = (k >> 5) & 127;
        
        int nArgs = k & 31;
        if (nArgs == 31) {
        	// it's a long form command
        	nArgs = makeUInt(16);
        	
        	// note: we don't support partitioned data here
        	assert ((nArgs & (1 << 15)) == 0);
        }
        
        // copy all the remaining arguments in an array
        byte commandArguments[] = new byte[nArgs];
        int c = 0;
        while (c < nArgs) {
        	commandArguments[c++] = makeByte();
        }
        
        this.embeddedCommand = readCommand(new DataInputStream(new ByteArrayInputStream(
				commandArguments)), elementClass, elementId, commandArguments.length);
    }
	
    @Override
	public void paint(CGMDisplay d) {
    	this.embeddedCommand.paint(d);
	}

	@Override
	public String toString() {
    	StringBuilder sb = new StringBuilder();
    	sb.append("MetafileDefaultsReplacement ");
    	sb.append(this.embeddedCommand.toString());
    	return sb.toString();
    }
}

/*
 * vim:encoding=utf8
 */
