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
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.io.DataInput;
import java.io.IOException;


/**
 * Class=4, Element=7
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class PolygonElement extends Command {
	protected final Path2D.Double polygon;

    public PolygonElement(int ec, int eid, int l, DataInput in)
            throws IOException {

        super(ec, eid, l, in);

        assert (this.args.length - this.currentArg) % sizeOfPoint() == 0;
        int n = (this.args.length - this.currentArg) / sizeOfPoint();

        this.polygon = new Path2D.Double(Path2D.WIND_EVEN_ODD);

        Point2D.Double p = makePoint();
        this.polygon.moveTo(p.x, p.y);

        for (int i = 1; i < n; i++) {
        	p = makePoint();
        	this.polygon.lineTo(p.x, p.y);
        }

        this.polygon.closePath();

        // make sure all the arguments were read
        assert (this.currentArg == this.args.length);
    }

    @Override
	public String toString() {
    	StringBuilder sb = new StringBuilder();
    	sb.append("Polygon ");
    	sb.append(printShape(this.polygon));
        return sb.toString();
    }

	@Override
	public void paint(CGMDisplay d) {
    	Graphics2D g2d = d.getGraphics2D();

    	d.fill(this.polygon);

    	if (d.drawEdge()) {
    		g2d.setColor(d.getEdgeColor());
    		g2d.setStroke(d.getEdgeStroke());
    		g2d.draw(this.polygon);
    	}
    }
}

/*
 * vim:encoding=utf8
 */
