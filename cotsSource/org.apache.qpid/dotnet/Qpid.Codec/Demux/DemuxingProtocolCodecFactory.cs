/*
 *
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
 *
 */
using System;
using System.Collections;
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Codec.Demux
{
    public class DemuxingProtocolCodecFactory : IProtocolCodecFactory
    {
        private ArrayList _decoderFactories = new ArrayList();
        private ArrayList _encoderFactories = new ArrayList();
        
        public void Register(Type encoderOrDecoderClass)
        {
            if (encoderOrDecoderClass == null)
            {
                throw new ArgumentNullException("encoderOrDecoderClass");
            }

            bool registered = false;
            if (typeof(IMessageEncoder).IsAssignableFrom(encoderOrDecoderClass))
            {
                Register(new DefaultConstructorMessageEncoderFactory(encoderOrDecoderClass));
                registered = true;
            }
            
            if (typeof(IMessageDecoder).IsAssignableFrom(encoderOrDecoderClass))
            {
                Register(new DefaultConstructorMessageDecoderFactory(encoderOrDecoderClass));
                registered = true;
            }
            
            if (!registered)
            {
                throw new ArgumentException("Unregisterable type: " + encoderOrDecoderClass);
            }
        }
        
        public void Register(IMessageEncoder encoder)
        {
            Register(new SingletonMessageEncoderFactory(encoder));
        }
        
        public void Register(IMessageEncoderFactory factory)
        {
            if (factory == null)
            {
                throw new ArgumentNullException("factory");
            }

            _encoderFactories.Add(factory);
        }
        
        public void Register(IMessageDecoder decoder)
        {
            Register(new SingletonMessageDecoderFactory(decoder));
        }
        
        public void Register(IMessageDecoderFactory factory)
        {
            if (factory == null)
            {
                throw new ArgumentNullException("factory");
            }
            _decoderFactories.Add(factory);
        }
        
        public IProtocolEncoder Encoder
        {
            get
            {
                return new ProtocolEncoderImpl(this);
            }
        }
        
        public IProtocolDecoder Decoder
        {
            get
            {
                return new ProtocolDecoderImpl(this);
            }            
        }
        
        protected void DisposeCodecResources()
        {
            // Do nothing by default
        }

        private class ProtocolEncoderImpl : IProtocolEncoder
        {
            private readonly Hashtable _encoders = new Hashtable();

            private DemuxingProtocolCodecFactory _enclosing;

            public ProtocolEncoderImpl(DemuxingProtocolCodecFactory enclosing)
            {
                _enclosing = enclosing;
                ArrayList encoderFactories = enclosing._encoderFactories;
                for (int i = encoderFactories.Count - 1; i >= 0; i--)
                {
                    IMessageEncoder encoder = ((IMessageEncoderFactory)encoderFactories[i]).NewEncoder();
                    foreach (Type type in encoder.MessageTypes.Keys)
                    {
                        _encoders[type] = encoder;
                    }
                }
            }

            public void Encode(object message, IProtocolEncoderOutput output)
            {
                Type type = message.GetType();
                IMessageEncoder encoder = FindEncoder(type);
                if (encoder == null)
                {
                    throw new ProtocolEncoderException("Unexpected message type: " + type);
                }

                encoder.Encode(message, output);
            }

            private IMessageEncoder FindEncoder(Type type)
            {
                IMessageEncoder encoder = (IMessageEncoder)_encoders[type];
                if (encoder == null)
                {
                    encoder = FindEncoder(type, new Hashtable());
                }

                return encoder;
            }

            private IMessageEncoder FindEncoder(Type type, Hashtable triedClasses)
            {
                IMessageEncoder encoder;

                if (triedClasses.Contains(type))
                {
                    return null;
                }
                triedClasses[type] = 1;

                encoder = (IMessageEncoder)_encoders[type];
                if (encoder == null)
                {
                    encoder = FindEncoder(type, triedClasses);
                    if (encoder != null)
                    {
                        return encoder;
                    }

                    Type[] interfaces = type.GetInterfaces();
                    for (int i = 0; i < interfaces.Length; i++)
                    {
                        encoder = FindEncoder(interfaces[i], triedClasses);
                        if (encoder != null)
                        {
                            return encoder;
                        }
                    }

                    return null;
                }
                else
                    return encoder;
            }

            public void Dispose()
            {
                _enclosing.DisposeCodecResources();
            }
        }
        
        private class ProtocolDecoderImpl : CumulativeProtocolDecoder
        {
            private readonly IMessageDecoder[] _decoders;
            private IMessageDecoder _currentDecoder;
            private DemuxingProtocolCodecFactory _enclosing;
            
            public ProtocolDecoderImpl(DemuxingProtocolCodecFactory enclosing)
            {
                _enclosing = enclosing;
                ArrayList decoderFactories = _enclosing._decoderFactories;
                _decoders = new IMessageDecoder[decoderFactories.Count];
                for (int i = decoderFactories.Count - 1; i >= 0; i--)
                {
                    _decoders[i] = ((IMessageDecoderFactory) decoderFactories[i]).NewDecoder();
                }
            }

            protected override bool DoDecode(ByteBuffer input, IProtocolDecoderOutput output)
            {
                MessageDecoderResult result;
                if (_currentDecoder == null)
                {
                    IMessageDecoder[] decoders = _decoders;
                    int undecodables = 0;
                
                    for (int i = decoders.Length - 1; i >= 0; i --) 
                    {
                        IMessageDecoder decoder = decoders[i];
                        int limit = input.Limit;
                        int pos = input.Position;
                        
                        try
                        {
                            result = decoder.Decodable(input);
                        }
                        finally
                        {
                            input.Position = pos;
                            input.Limit = limit;
                        }
                        
                        if (result == MessageDecoderResult.OK)
                        {
                            _currentDecoder = decoder;
                            break;
                        }
                        else if(result == MessageDecoderResult.NOT_OK)
                        {
                            undecodables ++;
                        }
                        else if (result != MessageDecoderResult.NEED_DATA)
                        {
                            throw new Exception("Unexpected decode result (see your decodable()): " + result);
                        }
                    }
                    
                    if (undecodables == _decoders.Length)
                    {
                        // Throw an exception if all decoders cannot decode data.
                        input.Position = input.Limit; // Skip data
                        throw new ProtocolDecoderException(
                            "No appropriate message decoder: " + input.GetHexDump());
                    }
                    
                    if (_currentDecoder == null)
                    {
                        // Decoder is not determined yet (i.e. we need more data)
                        return false;
                    }
                }
                
                result = _currentDecoder.Decode(input, output);
                if (result == MessageDecoderResult.OK)
                {
                    _currentDecoder = null;
                    return true;
                }
                else if (result == MessageDecoderResult.NEED_DATA)
                {
                    return false;
                }
                else if (result == MessageDecoderResult.NOT_OK) 
                {
                    throw new ProtocolDecoderException("Message decoder returned NOT_OK.");
                }
                else
                {
                    throw new Exception("Unexpected decode result (see your decode()): " + result);
                }
            }
        }
        
        private class SingletonMessageEncoderFactory : IMessageEncoderFactory
        {
            private readonly IMessageEncoder _encoder;
            
            public SingletonMessageEncoderFactory(IMessageEncoder encoder)
            {
                if (encoder == null)
                {
                    throw new ArgumentNullException("encoder");
                }
                _encoder = encoder;
            }

            public IMessageEncoder NewEncoder()
            {
                return _encoder;
            }
        }
        
        private class SingletonMessageDecoderFactory : IMessageDecoderFactory
        {
            private readonly IMessageDecoder _decoder;
            
            public SingletonMessageDecoderFactory(IMessageDecoder decoder)
            {
                if (decoder == null)
                {
                    throw new ArgumentNullException("decoder");
                }
                _decoder = decoder;
            }

            public IMessageDecoder NewDecoder()
            {
                return _decoder;
            }
        }
        
        private class DefaultConstructorMessageEncoderFactory : IMessageEncoderFactory
        {
            private readonly Type _encoderClass;
            
            public DefaultConstructorMessageEncoderFactory(Type encoderClass)
            {
                if (encoderClass == null)
                {
                    throw new ArgumentNullException("encoderClass");
                }
                
                if(!typeof(IMessageEncoder).IsAssignableFrom(encoderClass))
                {
                    throw new ArgumentException("encoderClass is not assignable to MessageEncoder");
                }
                _encoderClass = encoderClass;
            }

            public IMessageEncoder NewEncoder()
            {
                try
                {
                    return (IMessageEncoder) Activator.CreateInstance(_encoderClass);
                }
                catch (Exception e)
                {
                    throw new Exception( "Failed to create a new instance of " + _encoderClass, e);
                }
            }
        }

        private class DefaultConstructorMessageDecoderFactory : IMessageDecoderFactory
        {
            private readonly Type _decoderClass;
            
            public DefaultConstructorMessageDecoderFactory(Type decoderClass)
            {
                if (decoderClass == null)
                {
                    throw new ArgumentNullException("decoderClass");
                }
                
                if(!typeof(IMessageDecoder).IsAssignableFrom(decoderClass))
                {
                    throw new ArgumentException("decoderClass is not assignable to MessageDecoder");
                }
                _decoderClass = decoderClass;
            }

            public IMessageDecoder NewDecoder()
            {
                try
                {
                    return (IMessageDecoder) Activator.CreateInstance(_decoderClass);
                }
                catch (Exception e)
                {
                    throw new Exception("Failed to create a new instance of " + _decoderClass, e);
                }
            }
        }
    }
}



