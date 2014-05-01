/*
* Licensed to the Apache Software Foundation (ASF) under one
* or more contributor license agreements.  See the NOTICE file
* distributed with this work for additional information
* regarding copyright ownership.  The ASF licenses this file
* to you under the Apache License, Version 2.0 (the
* "License"); you may not use this file except in compliance
* with the License.  You may obtain a copy of the License at
*
*   http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
*/

#include <windows.h>
#include <msclr\lock.h>

#include "qpid/client/AsyncSession.h"
#include "qpid/framing/FrameSet.h"
#include "qpid/framing/AMQFrame.h"

#include "MessageBodyStream.h"

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Runtime::InteropServices;
using namespace System::Threading;
using namespace msclr;

using namespace qpid::client;
using namespace qpid::framing;

// Thefolowing def must match "Frames"  private typedef.
// TODO: make "Frames" publicly visible.
typedef qpid::InlineVector<AMQFrame, 4> FrameSetFrames;

using namespace std;

static void ThrowIfBadArgs (array<unsigned char>^ buffer, int offset, int count)
{
    if (buffer == nullptr)
	throw gcnew ArgumentNullException("buffer");

    if (offset < 0)
	throw gcnew ArgumentOutOfRangeException("offset");

    if (count < 0)
	throw gcnew ArgumentOutOfRangeException("count");

    if ((offset + count) > buffer->Length)
	throw gcnew ArgumentException("offset + count");
}


// Input stream constructor

MessageBodyStream::MessageBodyStream(FrameSet::shared_ptr *fspp)
{
    isInputStream = true;
    frameSetpp = fspp;
    fragmentCount = 0;
    length = 0;
    position = 0;
    currentFramep = NULL;

    const std::string *datap;	// pointer to the fragment's string variable that holds the content
    
    for(FrameSetFrames::const_iterator i = (*frameSetpp)->begin(); i != (*frameSetpp)->end(); i++) {
	if (i->getBody()->type() == CONTENT_BODY) {
	    fragmentCount++;
	    datap = &(i->castBody<AMQContentBody>()->getData());
	    length += datap->size();
	}
    }

    // fragmentCount can be zero for an empty message

    fragmentIndex = 0;
    fragmentPosition = 0;

    if (fragmentCount == 0) {
	currentFragment = NULL;
	fragmentLength = 0;
    }
    else if (fragmentCount == 1) {
	currentFragment = datap->data();
	fragmentLength = (int) length;
    }
    else {
	fragments = gcnew array<IntPtr>(fragmentCount);
	fragmentIndex = 0;
	for(FrameSetFrames::const_iterator i = (*frameSetpp)->begin(); i != (*frameSetpp)->end(); i++) {
	    if (i->getBody()->type() == CONTENT_BODY) {
		datap = &(i->castBody<AMQContentBody>()->getData());
		fragments[fragmentIndex++] = (IntPtr) (void *) datap;
	    }
	}
	fragmentIndex = 0;
	datap = (const std::string *) fragments[0].ToPointer();
	currentFragment = datap->data();
	fragmentLength = datap->size();
    }
}


int MessageBodyStream::Read(array<unsigned char>^ buffer, int offset, int count)
{
    if (!isInputStream)
	throw gcnew NotSupportedException();
    if (disposed)
	throw gcnew ObjectDisposedException("Stream");
    if (count == 0)
	return 0;
    ThrowIfBadArgs(buffer, offset, count);

    int nRead = 0;
    int remaining = count;

    while (nRead < count) {
	int fragAvail = fragmentLength - fragmentPosition;
	int copyCount = min (fragAvail, remaining);
	if (copyCount == 0) {
	    // no more to read
	    return nRead;
	}

	// copy from native space
	IntPtr nativep = (IntPtr) (void *) (currentFragment + fragmentPosition);
	Marshal::Copy (nativep, buffer, offset, copyCount);
	nRead += copyCount;
	remaining -= copyCount;
	fragmentPosition += copyCount;
	offset += copyCount;

	// advance to next fragment?
	if (fragmentPosition == fragmentLength) {
	    if (++fragmentIndex < fragmentCount) {
		const std::string *datap = (const std::string *) fragments[fragmentIndex].ToPointer();
		currentFragment = datap->data();
		fragmentLength = datap->size();
		fragmentPosition = 0;
	    }
	}
    }
    
    return nRead;
}


void MessageBodyStream::pushCurrentFrame(bool lastFrame)
{
    // set flags as in SessionImpl::sendContent.
    if (currentFramep->getBody()->type() == CONTENT_BODY) {

	if ((fragmentCount == 1) && lastFrame) {
	    // only one content frame
	    currentFramep->setFirstSegment(false);
	}
	else {
	    currentFramep->setFirstSegment(false);
	    currentFramep->setLastSegment(true);
	    if (fragmentCount != 1) {
		currentFramep->setFirstFrame(false);
	    }
	    if (!lastFrame) {
		currentFramep->setLastFrame(false);
	    }
	}
    }
    else {
	// the header frame
	currentFramep->setFirstSegment(false);
	if (!lastFrame) {
	    // there will be at least one content frame
	    currentFramep->setLastSegment(false);
	}
    }

    // add to frame set.  This makes a copy and ref counts the body
    (*frameSetpp)->append(*currentFramep);

    delete currentFramep;

    currentFramep = NULL;
}
    

IntPtr MessageBodyStream::GetFrameSet()
{
    if (currentFramep != NULL) {
	// No more content.  Tidy up the pending (possibly single header) frame.
	pushCurrentFrame(true);
    }

    if (frameSetpp == NULL) {
	return (IntPtr) NULL;
    }

    // shared_ptr.get()
    return (IntPtr) (void *) (*frameSetpp).get();
}

IntPtr MessageBodyStream::GetHeader()
{
    return (IntPtr) headerBodyp;
}


// Ouput stream constructor

MessageBodyStream::MessageBodyStream(int maxFrameSize)
{
    isInputStream = false;

    maxFrameContentSize = maxFrameSize - AMQFrame::frameOverhead();
    SequenceNumber unused;	// only meaningful on incoming frames
    frameSetpp = new FrameSet::shared_ptr(new FrameSet(unused));
    fragmentCount = 0;
    length = 0;
    position = 0;

    // header goes first in the outgoing frameset

    boost::intrusive_ptr<AMQBody> headerBody(new AMQHeaderBody);
    currentFramep = new AMQFrame(headerBody);
    headerBodyp = static_cast<AMQHeaderBody*>(headerBody.get());

    // mark this header frame as "full" to force the first write to create a new content frame
    fragmentPosition = maxFrameContentSize;
}

void MessageBodyStream::Write(array<unsigned char>^ buffer, int offset, int count)
{
    if (isInputStream)
	throw gcnew NotSupportedException();
    if (disposed)
	throw gcnew ObjectDisposedException("Stream");
    if (count == 0)
	return;
    ThrowIfBadArgs(buffer, offset, count);

    if (currentFramep == NULL) {
	// GetFrameSet() has been called and we no longer exclusively own the underlying frames.
	throw gcnew InvalidOperationException ("Mesage Body output already completed");
    }

    if (count <= 0)
	return;

    // keep GC memory movement at bay while copying to native space
    pin_ptr<unsigned char> pinnedBuf = &buffer[0];

    string *datap;

    int remaining = count;
    while (remaining > 0) {
	if (fragmentPosition == maxFrameContentSize) {
	    // move to a new frame, but not until ready to add new content.
	    // zero content is valid, or the final write may exactly fill to maxFrameContentSize

	    pushCurrentFrame(false);

	    currentFramep = new AMQFrame(AMQContentBody());
	    fragmentPosition = 0;
	    fragmentCount++;
	}

	int copyCount = min (remaining, (maxFrameContentSize - fragmentPosition));
	datap = &(currentFramep->castBody<AMQContentBody>()->getData());

	char *outp = (char *) pinnedBuf + offset;
	if (fragmentPosition == 0) {
	    datap->assign(outp, copyCount);
	}
	else {
	    datap->append(outp, copyCount);
	}

	position += copyCount;
	fragmentPosition += copyCount;
	remaining -= copyCount;
	offset += copyCount;
    }
}


void MessageBodyStream::Cleanup()
{
    {
        lock l(this);
        if (disposed)
	    return;

	disposed = true;
    }
  
    try {}
    finally
    {
        if (frameSetpp != NULL) {
	    delete frameSetpp;
	    frameSetpp = NULL;
	}
	if (currentFramep != NULL) {
	    delete currentFramep;
	    currentFramep = NULL;
	}
    }
}

MessageBodyStream::~MessageBodyStream()
{
    Cleanup();
}

MessageBodyStream::!MessageBodyStream()
{
    Cleanup();
}

void MessageBodyStream::Close()
{
    // Simulate Dispose()...
    Cleanup();
    GC::SuppressFinalize(this);
}


}}} // namespace Apache::Qpid::Interop
