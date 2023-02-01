<Query Kind="Program">
  <Namespace>Packet</Namespace>
  <Namespace>System.Net</Namespace>
  <Namespace>System.Net.Sockets</Namespace>
</Query>

void Main()
{
	Console.WriteLine($"Little endian bit converter: {BitConverter.IsLittleEndian}");
	var r = new Thread(Receiver);
	r.Start();
	r.Join();
}

void Receiver()
{
	var ServerEndPoint = new IPEndPoint(IPAddress.Any, 9500);
	var socket = new Socket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp);
	socket.Bind(ServerEndPoint);

	var sender = new IPEndPoint(IPAddress.Any, 0);
	var Remote = (EndPoint)(sender);

	byte[] data = new byte[65535];
	int counter = 0;
	while (true)
	{
		int recv = socket.ReceiveFrom(data, ref Remote);
		Console.WriteLine($"Received: {counter++}");
		// do something with the data
		if (result != null)
		{
			Console.WriteLine("High Val: " + (result.MessageType & 0xFF));
			Console.WriteLine("Low Val: " + ((result.MessageType >> 8) & 0xFF));
		}
	}
}


