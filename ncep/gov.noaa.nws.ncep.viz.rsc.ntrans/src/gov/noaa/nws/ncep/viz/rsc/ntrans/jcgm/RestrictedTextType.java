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

import java.io.*;


/**
 * Class=5, Element=42
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class RestrictedTextType extends Command {
	enum Type {
		BASIC, BOXED_CAP, BOXED_ALL, ISOTROPIC_CAP, ISOTROPIC_ALL, JUSTIFIED
	}

	static private Type type;
	
	static {
		reset();
	}

	public RestrictedTextType(int ec, int eid, int l, DataInput in) throws IOException {
		super(ec, eid, l, in);

		int typ = makeIndex();
		switch (typ) {
		case 1:
			type = Type.BASIC;
			break;
		case 2:
			type = Type.BOXED_CAP;
			break;
		case 3:
			type = Type.BOXED_ALL;
			break;
		case 4:
			type = Type.ISOTROPIC_CAP;
			break;
		case 5:
			type = Type.ISOTROPIC_ALL;
			break;
		case 6:
			type = Type.JUSTIFIED;
			break;
		default:
			type = Type.BASIC;
			unsupported("unsupported text type "+typ);
		}
		
        // make sure all the arguments were read
        assert (this.currentArg == this.args.length);
	}

	public static void reset() {
		type = Type.BASIC;
	}

	public static Type getType() {
		return type;
	}

	@Override
	public String toString() {
		return "RestrictedTextType " + type;
	}
}

/*
 * vim:encoding=utf8
 */
