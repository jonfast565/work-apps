<Query Kind="Statements" />

int ClosestMultiple(int number, int closestTo)
{	
	if (closestTo > number)
		return closestTo;

	number = number + closestTo / 2;
	number = number - (number % closestTo);
	return number;
}

Console.WriteLine(ClosestMultiple(1056, 512));