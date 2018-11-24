


// msgs_ack#62d6b459 msg_ids:Vector<Vector<long>> = MsgsAck;
// =>            { name: 'msg_ids',
//                  type: [ 'Vector', 'Vector', 'long' ],
//                  optional: false }


true#3fedd339 = True;

//boolFalse#bc799737 = Bool;
//boolTrue#997275b5 = Bool;

//inputPeerEmpty#7f3b18ea = InputPeer;
//inputPeerSelf#7da07ec9 = InputPeer;
//inputPeerChat#179be863 chat_id:int = InputPeer;
//inputPeerUser#7b8e7de6 user_id:int access_hash:long = InputPeer;
//inputPeerChannel#20adaef8 channel_id:int access_hash:long = InputPeer;

//a#00000000 b:B = A;
//b#00000001 d:D = B;
//c#00000002 b2:B = C;
//d#00000003 arg0:int = D;
//e#00000004 arg1:Vector<A> = E;
//f#00000005 arg2:string = Vector<long>;

---functions---

//invokeAfterMsgs#3dc4b4f0 {X:Type} msg_ids:Vector<long> query:!X = X;

invokeAfterMsgs#3dc4b4f0 {X:Type} msg_ids:Vector<long> query:!X = Vector<X>;

invokeAfterMsgs2#3dc4b4f5 {X:Type} msg_ids:Vector<long> query:!X = X;

//invokeAfterMsgs#3dc4b4f0 {X:Type} flags:# a:int b:flags.4?string msg_ids:Vector<long> query:X = Bool;

flagsTest#10000001 flags:# w:int a:flags.0?string w2:string b:flags.1?true bb:flags.22?true = Bool;
