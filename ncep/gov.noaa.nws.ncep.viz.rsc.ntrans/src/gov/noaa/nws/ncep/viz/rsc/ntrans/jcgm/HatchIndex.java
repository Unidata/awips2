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
 * Class=5, Element=24
 * @author xphc (Philippe Cadé)
 * @version $Id$
 * @since Jun 10, 2009
 */
public class HatchIndex extends Command {
	enum HatchType {
		HORIZONTAL_LINES, VERTICAL_LINES, POSITIVE_SLOPE_LINES, NEGATIVE_SLOPE_LINES,
		HORIZONTAL_VERTICAL_CROSSHATCH, POSITIVE_NEGATIVE_CROSSHATCH
	}
	
	HatchType type = HatchType.HORIZONTAL_LINES;

	public HatchIndex(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);
		
		int index = makeIndex();
		switch (index) {
		case 1:
			this.type = HatchType.HORIZONTAL_LINES;
			break;
		case 2:
			this.type = HatchType.VERTICAL_LINES;
			break;
		case 3:
			this.type = HatchType.POSITIVE_SLOPE_LINES;
			break;
		case 4:
			this.type = HatchType.NEGATIVE_SLOPE_LINES;
			break;
		case 5:
			this.type = HatchType.HORIZONTAL_VERTICAL_CROSSHATCH;
			break;
		case 6:
			this.type = HatchType.POSITIVE_NEGATIVE_CROSSHATCH;
			break;
		default:
			unsupported("hatch style: " + index);
		}
	}

	@Override
	public void paint(CGMDisplay d) {
		d.setHatchStyle(this.type);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("HatchIndex ").append(this.type);
		return sb.toString();
	}
}

/*
 * vim:encoding=utf8
 */
