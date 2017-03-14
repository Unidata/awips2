
// +++ 	Name: hourglass.H
//Purpose: This is a header file for cusor pixmap creation. 
// ---*************************************************************************
/*---------------------------------------------------------------------
 * $Date: 2004/12/20 15:52:22 $             $Revision: 1.2 $
 *---------------------------------------------------------------------
 * 
 *
 *             Copyright (c) 1992, Visual Edge Software Ltd.
 *
 *  ALL  RIGHTS  RESERVED.  Permission  to  use,  copy,  modify,  and
 *  distribute  this  software  and its documentation for any purpose
 *  and  without  fee  is  hereby  granted,  provided  that the above
 *  copyright  notice  appear  in  all  copies  and  that  both  that
 *  copyright  notice and this permission notice appear in supporting
 *  used  in advertising  or publicity  pertaining to distribution of
 *  the software without specific, written prior permission. The year
 *  included in the notice is the year of the creation of the work.
 *
 *  VISUAL EDGE SOFTWARE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 *  SOFTWARE,  INCLUDING  ALL IMPLIED  WARRANTIES OF  MERCHANTABILITY
 *  AND  FITNESS FOR A  PARTICULAR PURPOSE.  IN NO EVENT SHALL VISUAL
 *  EDGE   SOFTWARE   BE   LIABLE   FOR  ANY  SPECIAL,   INDIRECT  OR
 *  CONSEQUENTIAL  DAMAGES  OR  ANY DAMAGES WHATSOEVER RESULTING FROM
 *  LOSS  OF  USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 *  NEGLIGENCE  OR  OTHER  TORTIOUS  ACTION,  ARISING  OUT  OF  OR IN
 *  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *--------------------------------------------------------------------------*/
#define hourglass_width 32
#define hourglass_height 32
#define hourglass_x_hot 15
#define hourglass_y_hot 15
static char hourglass_bits[] = {
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0x40, 0x00, 0x00, 0x01,
   0xe0, 0xff, 0xff, 0x03, 0x60, 0x00, 0x00, 0x03, 0x60, 0x00, 0x00, 0x03,
   0x60, 0x00, 0x00, 0x03, 0x60, 0xc0, 0x01, 0x03, 0xc0, 0xa8, 0x8e, 0x01,
   0x80, 0x55, 0xdd, 0x00, 0x00, 0xab, 0x6f, 0x00, 0x00, 0x56, 0x37, 0x00,
   0x00, 0xec, 0x1b, 0x00, 0x00, 0xd8, 0x0d, 0x00, 0x00, 0xb0, 0x06, 0x00,
   0x00, 0xa0, 0x02, 0x00, 0x00, 0x20, 0x02, 0x00, 0x00, 0xb0, 0x06, 0x00,
   0x00, 0x98, 0x0c, 0x00, 0x00, 0x8c, 0x18, 0x00, 0x00, 0x06, 0x30, 0x00,
   0x00, 0x83, 0x60, 0x00, 0x80, 0xc1, 0xc3, 0x00, 0xc0, 0xb0, 0x86, 0x01,
   0x60, 0x5c, 0x1d, 0x03, 0x60, 0x2a, 0x39, 0x03, 0x60, 0xdd, 0x7f, 0x03,
   0x60, 0x00, 0x00, 0x03, 0xe0, 0xff, 0xff, 0x03, 0x40, 0x00, 0x00, 0x01,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03};
