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
 * Class=5, Element=18
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class TextAlignment extends Command {
	protected enum HorizontalAlignment { NORMAL_HORIZONTAL, LEFT, CENTRE, RIGHT, CONTINOUS_HORIZONTAL }
	protected enum VerticalAlignment { NORMAL_VERTICAL, TOP, CAP, HALF, BASE, BOTTOM, CONTINOUS_VERTICAL }

	protected HorizontalAlignment horizontalAlignment;
	protected VerticalAlignment verticalAlignment;
	//ORIGINAL//private double continuousHorizontalAlignment;
	private final double continuousHorizontalAlignment; //TODO:why?
	//ORIGINAL//private double continuousVerticalAlignment;
	private final double continuousVerticalAlignment; //TODO:why?

	//ORIGINAL DOES NOT CONTAIN...
	@Override
	protected int makeEnum() {
		return makeSignedInt8();
	}

	//ORIGINAL DOES NOT CONTAIN...
	@Override
	protected int sizeOfEnum() {
		return 1;
	}

	public TextAlignment(int ec, int eid, int l, DataInput in)
	throws IOException {
		super(ec, eid, l, in);

		int horiz = makeEnum();
		switch (horiz) {
		case 0:
			this.horizontalAlignment = HorizontalAlignment.NORMAL_HORIZONTAL;
			break;
		case 1:
			this.horizontalAlignment = HorizontalAlignment.LEFT;
			break;
		case 2:
			this.horizontalAlignment = HorizontalAlignment.CENTRE;
			break;
		case 3:
			this.horizontalAlignment = HorizontalAlignment.RIGHT;
			break;
		case 4:
			this.horizontalAlignment = HorizontalAlignment.CONTINOUS_HORIZONTAL;
			break;
		default:
			this.horizontalAlignment = HorizontalAlignment.NORMAL_HORIZONTAL;
			unsupported("unsupported horizontal alignment "+horiz);
		}

		int vert = makeEnum();
		switch (vert) {
		case 0:
			this.verticalAlignment = VerticalAlignment.NORMAL_VERTICAL;
			break;
		case 1:
			this.verticalAlignment = VerticalAlignment.TOP;
			break;
		case 2:
			this.verticalAlignment = VerticalAlignment.CAP;
			break;
		case 3:
			this.verticalAlignment = VerticalAlignment.HALF;
			break;
		case 4:
			this.verticalAlignment = VerticalAlignment.BASE;
			break;
		case 5:
			this.verticalAlignment = VerticalAlignment.BOTTOM;
			break;
		case 6:
			this.verticalAlignment = VerticalAlignment.CONTINOUS_VERTICAL;
			break;
		default:
			this.verticalAlignment = VerticalAlignment.NORMAL_VERTICAL;
			unsupported("unsupported vertical alignment "+vert);
		}

		//ORIGINAL//this.continuousHorizontalAlignment = makeReal();
		this.continuousHorizontalAlignment = 0.0;
		//ORIGINAL//this.continuousVerticalAlignment = makeReal();
		this.continuousVerticalAlignment = 0.0;

		// make sure all the arguments were read
		assert (this.currentArg == this.args.length);
	}

	@Override
	public void paint(CGMDisplay d) {
		d.setTextAlignment(this.horizontalAlignment, this.verticalAlignment,
				this.continuousHorizontalAlignment, this.continuousVerticalAlignment);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("TextAlignment");
		sb.append(" horizontal=").append(this.horizontalAlignment);
		sb.append(" vertical=").append(this.verticalAlignment);
		sb.append(" continousHorizontalAlignment=").append(this.continuousHorizontalAlignment);
		sb.append(" continuousVerticalAlignment=").append(this.continuousVerticalAlignment);
		return sb.toString();
	}
}

/*
 * vim:encoding=utf8
 */
