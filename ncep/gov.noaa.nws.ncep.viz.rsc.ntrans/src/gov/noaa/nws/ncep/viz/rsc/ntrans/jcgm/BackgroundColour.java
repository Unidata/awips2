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

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.DataInput;
import java.io.IOException;


/**
 * Class=2, Element=7
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class BackgroundColour extends Command {
	private final Color backgroundColor;

	public BackgroundColour(int ec, int eid, int l, DataInput in)
			throws IOException {
		super(ec, eid, l, in);

		this.backgroundColor = makeDirectColor();

		// make sure all the arguments were read
		assert (this.currentArg == this.args.length);
	}

	@Override
	public void paint(CGMDisplay d) {
		// FIXME: the specification says that the setting of the background
		// color should happen at Picture Body Begin, not here
		if (!d.isTransparent() && !d.isViewCleared()) {
			Graphics2D g2d = d.getGraphics2D();
			g2d.setColor(this.backgroundColor);

			Point2D.Double[] extent = d.getExtent();
			g2d.fill(new Rectangle2D.Double(extent[0].x, extent[0].y,
					extent[1].x - extent[0].x, extent[1].y - extent[0].y));
			d.setViewCleared(true);
		}
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("BackgroundColour ").append(this.backgroundColor);
		return sb.toString();
	}
}

/*
 * vim:encoding=utf8
 */
