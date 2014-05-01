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
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public abstract class CapCommand extends Command {

	protected LineCapIndicator lineIndicator;
	protected DashCapIndicator dashIndicator;

	public CapCommand(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);
        int lineIndic = makeIndex();
		switch (lineIndic) {
        
        case 1:
        	this.lineIndicator = LineCapIndicator.UNSPECIFIED;
        	break;
        case 2:
        	this.lineIndicator = LineCapIndicator.BUTT;
        	break;
        case 3:
        	this.lineIndicator = LineCapIndicator.ROUND;
        	break;
        case 4:
        	this.lineIndicator = LineCapIndicator.PROJECTED_SQUARE;
        	unsupported("unsupported line cap indicator "+lineIndic);
        	break;
        case 5:
        	this.lineIndicator = LineCapIndicator.TRIANGLE;
        	unsupported("unsupported line cap indicator "+lineIndic);
        	break;
        default:
        	unsupported("unsupported line cap indicator "+lineIndic);
        	this.lineIndicator = LineCapIndicator.UNSPECIFIED;
        }
        
		int dashIndic = makeIndex();
        switch (dashIndic) {
        case 1:
        	this.dashIndicator = DashCapIndicator.UNSPECIFIED;
        	break;
        case 2:
        	this.dashIndicator = DashCapIndicator.BUTT;
        	break;
        case 3:
        	this.dashIndicator = DashCapIndicator.MATCH;
        	break;
        default:
        	unsupported("unsupported dash cap indicator "+dashIndic);
        	this.dashIndicator = DashCapIndicator.UNSPECIFIED;
        }
	}

}

/*
 * vim:encoding=utf8
 */
