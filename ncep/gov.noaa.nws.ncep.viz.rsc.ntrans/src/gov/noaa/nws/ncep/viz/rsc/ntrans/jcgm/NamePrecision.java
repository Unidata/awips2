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
 * Class=1, Element=16
 * @author xphc (Philippe Cadé)
 * @version $Id: IntegerPrecision.java 3 2009-10-16 08:51:15Z phica $
 */
//ORIGINAL//class NamePrecision extends Command {
public class NamePrecision extends Command {
	private static int precision;

	static {
		reset();
	}

	public NamePrecision(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);
		NamePrecision.precision = makeInt();

		assert (NamePrecision.precision == 8 || NamePrecision.precision == 16 ||
				NamePrecision.precision == 24 || NamePrecision.precision == 32) : "unsupported NAME PRECISION";
	}

	static void reset() {
		precision = 16;
	}

	static int getPrecision() {
		return NamePrecision.precision;
	}

	@Override
	public String toString() {
		String s = "NamePrecision " + String.valueOf(NamePrecision.precision);
		return s;
	}
}

/*
 * vim:encoding=utf8
 */
