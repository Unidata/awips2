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
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.io.*;



/**
 * Class=4, Element=12
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class CircleElement extends Command {
	protected Point2D.Double center;
	protected double radius;
	private Ellipse2D.Double shape;

    public CircleElement(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
        
        this.center = makePoint();
        this.radius = makeVdc();
        
        this.shape = new Ellipse2D.Double(this.center.x - this.radius, this.center.y - this.radius, 2 * this.radius, 2 * this.radius);
        
        // make sure all the arguments were read
        assert (this.currentArg == this.args.length);
    }

    @Override
	public String toString() {
        return "Circle [" + this.center.x + "," + this.center.y + "] " + this.radius;
    }

    @Override
	public void paint(CGMDisplay d) {
		Graphics2D g2d = d.getGraphics2D();

		d.fill(this.shape);
		
		if (d.drawEdge()) {
			g2d.setColor(d.getEdgeColor());
			g2d.setStroke(d.getEdgeStroke());
			g2d.draw(this.shape);
		}
	}
}

/*
 * vim:encoding=utf8
 */
