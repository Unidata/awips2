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

#pragma once

namespace Apache {
namespace Qpid {
namespace Interop {

using namespace System;
using namespace System::Runtime::InteropServices;

using namespace qpid::client;
using namespace qpid::framing;
using namespace std;


// This class provides memory streaming of the message body contents
// between native and managed space.  To avoid additional memory copies
// in native space, it reads and writes directly to the low level Qpid
// frames.

public ref class MessageBodyStream : System::IO::Stream
{
private:
    bool isInputStream;
    long long length;
    long long position;

    // the boost smart pointer that keeps the message body frames in memory
    FrameSet::shared_ptr *frameSetpp;

    int fragmentCount;
    int fragmentIndex;
    const char* currentFragment;
    int fragmentPosition;
    int fragmentLength;
    array<IntPtr>^ fragments;

    int maxFrameContentSize;
    AMQFrame* currentFramep;
    void* headerBodyp;
    bool disposed;
    bool finalizing;
    void Cleanup();

internal:
    // incoming message
    MessageBodyStream(FrameSet::shared_ptr *fspp);
    // outgoing message
    MessageBodyStream(int maxFrameSize);
    void pushCurrentFrame(bool last);
public:  
    ~MessageBodyStream();
    !MessageBodyStream();
    virtual void Close() override;
    virtual int Read(
	[InAttribute] [OutAttribute] array<unsigned char>^ buffer, 
	int offset, 
	int count) override;

    virtual void Write(
	array<unsigned char>^ buffer, 
	int offset, 
	int count) override;


    IntPtr GetFrameSet();
    IntPtr GetHeader();

    virtual void Flush() override {} // noop


    // TODO: see CanSeek below.
    virtual long long Seek(
	long long offset, 
	System::IO::SeekOrigin origin) override {throw gcnew System::NotSupportedException(); }

    // TODO: see CanSeek below.
    virtual void SetLength(
	long long value) override {throw gcnew System::NotSupportedException(); }

    virtual property long long Length {
	long long get() override { return length; }
    };

    virtual property long long Position {
	long long get() override { return position; }
	void set(long long p) override { throw gcnew System::NotSupportedException(); }
    };


    virtual property bool CanRead {
	bool get () override { return isInputStream; }
    }

    virtual property bool CanWrite {
	bool get () override { return !isInputStream; }
    }

    // Note:  this class must return true to signal that the Length property works.
    // Required by the raw message encoder.
    // "If a class derived from Stream does not support seeking, calls to Length,
    // SetLength, Position, and Seek throw a NotSupportedException".

    virtual property bool CanSeek {
	bool get () override { return true; }
    }

    virtual property bool CanTimeout {
	bool get () override { return isInputStream; }
    }
};

}}} // namespace Apache::Qpid::Interop
