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
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.io.DataInput;
import java.io.IOException;


/**
 * Class=4, Element=2
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class DisjointPolyline extends Command {
	private final Line2D.Double[] lines;

    public DisjointPolyline(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);

        int n = this.args.length / sizeOfPoint();
        assert (n % 2 == 0);

        this.lines = new Line2D.Double[n / 2];
        for (int i = 0; i < (n/2); i++) {
        	Point2D.Double p1 = makePoint();
        	Point2D.Double p2 = makePoint();
        	this.lines[i] = new Line2D.Double(p1.x, p1.y, p2.x, p2.y);
        }
    }

    @Override
	public String toString() {
    	StringBuilder sb = new StringBuilder();
    	sb.append("DisjointPolyline [");
        for (int i = 0; i < this.lines.length; i++) {
        	sb.append("(");
        	sb.append(this.lines[i].x1).append(",");
        	sb.append(this.lines[i].y1).append(",");
        	sb.append(this.lines[i].x2).append(",");
        	sb.append(this.lines[i].y2);
        	sb.append(")");
        }
        sb.append("]");
        return sb.toString();
    }

    @Override
	public void paint(CGMDisplay d) {
        Graphics2D g2d = d.getGraphics2D();
		g2d.setColor(d.getLineColor());
		g2d.setStroke(d.getLineStroke());

        for (int i = 0; i < this.lines.length; i++) {
        	g2d.draw(this.lines[i]);
        }
    }
}

/*
 * vim:encoding=utf8
 */
