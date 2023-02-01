<Query Kind="Statements" />

var data = new byte[3] {0xFF, 0xF1, 0x11};
var i = 0;
var first = data[i] >> 4 & 0x0F;
Console.WriteLine(first);
var second = data[i] & 0x0F;
Console.WriteLine(second);
var third = data[i + 1] >> 4 & 0x0F;
Console.WriteLine(third);
var fourth = data[i + 1] & 0x0F;
Console.WriteLine(fourth);
var fifth = data[i + 2] >> 4 & 0x0F;
Console.WriteLine(fifth);
var sixth = data[i + 2] & 0x0F;
Console.WriteLine(sixth);
var firstValue = (first << 8) + (second << 4) + third;
Console.WriteLine(firstValue);
var secondValue = (fourth << 8) + (fifth << 4) + sixth;
Console.WriteLine(secondValue);