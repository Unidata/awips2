/*
 * Copyright (c) 2010, Swiss AviationSoftware Ltd. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - Neither the name of the Swiss AviationSoftware Ltd. nor the names of its
 *   contributors may be used to endorse or promote products derived from this
 *   software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm;

import java.util.ArrayList;
import java.util.List;

/**
 * Structured Data Record container.
 * @version $Id:  $ 
 * @author  xphc
 * @since Oct 5, 2010
 */
public class StructuredDataRecord {
	enum StructuredDataType {
		SDR(1),
		CI(2),
		CD(3),
		N(4),
		E(5),
		I(6),
		RESERVED(7),
		IF8(8),
		IF16(9),
		IF32(10),
		IX(11),
		R(12),
		S(13),
		SF(14),
		VC(15),
		VDC(16),
		CCO(17),
		UI8(18),
		UI32(19),
		BS(20),
		CL(21),
		UI16(22)
		;

		private final int index;

		private StructuredDataType(int index) {
			this.index = index;
		}

		static StructuredDataType get(int index) {
			for (StructuredDataType type: values()) {
				if (type.index == index)
					return type;
			}
			throw new IllegalArgumentException("unknown index "+index);
		}
	}

	/**
	 * One entry in the structured data record
	 * @version $Id:  $ 
	 * @author  xphc
	 * @since Oct 5, 2010
	 */
	class Member {
		private final StructuredDataType type;
		private final int count;
		private final List<Object> data;

		Member(StructuredDataType type, int count, List<Object> data) {
			this.type = type;
			this.count = count;
			this.data = data;
		}

		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			builder.append("[type=");
			builder.append(this.type);
			builder.append(", count=");
			builder.append(this.count);
			builder.append(", data=");
			builder.append(this.data);
			builder.append("]");
			return builder.toString();
		}
	}

	private final List<Member> members = new ArrayList<Member>();

	void add(StructuredDataType type, int count, List<Object> data) {
		this.members.add(new Member(type, count, data));
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("StructuredDataRecord [members=");
		builder.append(this.members);
		builder.append("]");
		return builder.toString();
	}

}
