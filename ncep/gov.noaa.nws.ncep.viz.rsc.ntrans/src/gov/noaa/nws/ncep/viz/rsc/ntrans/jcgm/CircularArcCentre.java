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

import java.awt.Graphics2D;
import java.awt.geom.Arc2D;
import java.awt.geom.Point2D;
import java.io.DataInput;
import java.io.IOException;


/**
 * Class=4, Element=15
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class CircularArcCentre extends Command {
    protected Point2D.Double center;
	protected double startDeltaX;
	protected double startDeltaY;
	protected double endDeltaX;
	protected double endDeltaY;
	protected double radius;
	protected Arc2D.Double shape;

	public CircularArcCentre(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);

		this.center = makePoint();
		this.startDeltaX = makeVdc();
		this.startDeltaY = makeVdc();
		this.endDeltaX = makeVdc();
		this.endDeltaY = makeVdc();
		this.radius = makeVdc();

		DimensionDouble size = new DimensionDouble(2 * this.radius, 2 * this.radius);

		this.shape = new Arc2D.Double();
		this.shape.setFrame(new Point2D.Double(this.center.x - this.radius, this.center.y -
																			this.radius), size);
		// setAngles will draw the angle from the start point to the end point
		// in a counterclockwise manner. Since the AWT y axis is inverted, we're
		// giving the start point as being the end point here.
		this.shape.setAngles(this.center.x + this.endDeltaX, this.center.y + this.endDeltaY,
			this.center.x + this.startDeltaX, this.center.y + this.startDeltaY);
		this.shape.setArcType(Arc2D.OPEN);
	}

    @Override
	public void paint(CGMDisplay d) {
    	Graphics2D g2d = d.getGraphics2D();
    	g2d.setColor(d.getLineColor());
		g2d.setStroke(d.getLineStroke());
    	g2d.draw(this.shape);
	}

	@Override
	public String toString() {
        return "CircularArcCentre [" + this.center.x + "," + this.center.y + "] [" + this.startDeltaX + "," + this.startDeltaY + "] [" + this.endDeltaX
                + "," + this.endDeltaY + "] " + this.radius;
    }
}

/*
 * vim:encoding=utf8
 */
