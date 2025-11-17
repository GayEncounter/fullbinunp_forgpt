function RecStr(pref, n, digits)
{
	var ns = n.toString();
	var ret = pref;

	for(var z = ns.length; z < digits; z++)
		ret += '0';

	return ret + ns; 
}

// standard function parseInt doesn't handle numbers with highest bit set correctly (returns 0x7FFFFFFF), so here's my thingy to do that
function parseHex(str)
{
	var i = 0;
	var n = 0;
	
	while(i<str.length)
	{
		n *= 16;
		
		switch(str[i])
		{
			case '0': break;
			case '1': n += 1; break;
			case '2': n += 2; break;
			case '3': n += 3; break;
			case '4': n += 4; break;
			case '5': n += 5; break;
			case '6': n += 6; break;
			case '7': n += 7; break;
			case '8': n += 8; break;
			case '9': n += 9; break;
			case 'A': case 'a': n += 10; break;
			case 'B': case 'b': n += 11; break;
			case 'C': case 'c': n += 12; break;
			case 'D': case 'd': n += 13; break;
			case 'E': case 'e': n += 14; break;
			case 'F': case 'f': n += 15; break;
		}
		
		i++;
	}
	
	return n;
}

Object.defineProperties(_G, {
	'ENTITY_VER_28a': { value: 280, writable: false }, // Last Light build 2012-10-19
	'ENTITY_VER_28b': { value: 281, writable: false }, // Last Light build 2012-12-03
	'ENTITY_VER_29':  { value: 290, writable: false }, // Last Light release
	'ENTITY_VER_30':  { value: 300, writable: false }  // Redux release
});