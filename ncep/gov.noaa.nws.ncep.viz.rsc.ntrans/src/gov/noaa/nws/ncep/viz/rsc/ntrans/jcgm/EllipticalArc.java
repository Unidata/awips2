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
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Point2D;
import java.io.*;


/**
 * Class=2, Element=7
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class EllipticalArc extends EllipseElement {
	
    protected double startVectorDeltaX;
    protected double startVectorDeltaY;
    protected double endVectorDeltaX;
    protected double endVectorDeltaY;

	public EllipticalArc(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
        
        this.startVectorDeltaX = makeVdc();
        this.startVectorDeltaY = makeVdc();
        this.endVectorDeltaX = makeVdc();
        this.endVectorDeltaY = makeVdc();
    }

	@Override
	protected Shape createShape(double centerFirstDistance, double centerSecondDistance) {
		return new Arc2D.Double(-centerFirstDistance,
			-centerSecondDistance , 2 * centerFirstDistance,
			2 * centerSecondDistance, 0, 360, getClosureType());
	}

	protected int getClosureType() {
		return Arc2D.OPEN;
	}
	
	/**
	 * {@inheritDoc)
	 * 
	 * From the specification:
	 * <p>
	 * The defined arc begins at the intersection of the ellipse and the start
	 * ray and follows the ellipse to the intersection of the ellipse and the
	 * end ray in the direction defined as follows. A "conjugate radius" is
	 * defined to be half of a conjugate diameter. Letting the centre point be
	 * labelled M, the first CDP end point P1, and the second CDP end point P2,
	 * then the line segments M-P1 and M-P2 define two conjugate radii, referred
	 * to in what follows as the first conjugate radius and the second conjugate
	 * radius respectively. The conjugate radii meet at M and define two angles:
	 * the sum of the two angles is 360°, one angle is less than 180° and the
	 * other is greater than 180°. The drawing direction of the elliptical arc
	 * is the direction from the first conjugate radius to the second conjugate
	 * radius through the smaller of these two angles.
	 * </p>
	 */
	@Override
	protected void applyArcs(CGMDisplay d, AffineTransform transform) {
		Arc2D.Double arc = (Arc2D.Double)this.ellipse;
		
		Point2D.Double startVector = new Point2D.Double();
		Point2D.Double endVector = new Point2D.Double();
		transform.transform(new Point2D.Double(this.startVectorDeltaX, this.startVectorDeltaY), startVector);
		transform.transform(new Point2D.Double(this.endVectorDeltaX, this.endVectorDeltaY), endVector);
		
		double firstConjugateAngle = d.angle(this.firstConjugateDiameterEndPoint.x - this.center.x, this.firstConjugateDiameterEndPoint.y - this.center.y);
		double secondConjugateAngle = d.angle(this.secondConjugateDiameterEndPoint.x - this.center.x, this.secondConjugateDiameterEndPoint.y - this.center.y);
		
		if (firstConjugateAngle > secondConjugateAngle) {
			if (firstConjugateAngle - secondConjugateAngle < Math.PI) {
				arc.setAngles(startVector, endVector);
			}
			else {
				arc.setAngles(endVector, startVector);
			}
		}
		else {
			if (secondConjugateAngle - firstConjugateAngle < Math.PI) {
				arc.setAngles(endVector, startVector);
			}
			else {
				arc.setAngles(startVector, endVector);
			}
		}
	}

	@Override
	public void paint(CGMDisplay d) {
		initializeShape(d);
		
		Graphics2D g2d = d.getGraphics2D();
		g2d.setColor(d.getLineColor());
		g2d.setStroke(d.getLineStroke());
		g2d.draw(this.ellipse);
	}

	@Override
	public String toString() {
        return "EllipticalArc [" + this.center.x + "," + this.center.y + "] [" +
				this.firstConjugateDiameterEndPoint.x + "," +
				this.firstConjugateDiameterEndPoint.y + "] [" +
				this.secondConjugateDiameterEndPoint.x + "," +
				this.secondConjugateDiameterEndPoint.y + "] [" +
				this.startVectorDeltaX + "," +
				this.startVectorDeltaY + "] [" +
				this.endVectorDeltaX + "," +
				this.endVectorDeltaY + "]";
    }
}

/*
 * vim:encoding=utf8
 */
