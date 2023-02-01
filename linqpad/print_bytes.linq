<Query Kind="Program" />

void Main()
{
	var s = new byte[] { 00, 01, 02, 03, 04, 05, 06, 07 };
	PrintByteArray(s);
}

void PrintByteArray(byte[] data, int columns = 8)
{
	var counter = 0;
	foreach (var item in data)
	{
		Console.Write($"{item:X2} ");
		counter++;
		if (counter % columns == 0)
		{
			counter = 0;
			Console.Write(Environment.NewLine);
		}
	}
}

// You can define other methods, fields, classes and namespaces here
