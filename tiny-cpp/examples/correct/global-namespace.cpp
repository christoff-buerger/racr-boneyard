// This program and the accompanying materials are made available under the
// terms of the MIT license (X11 license) which accompanies this distribution.

// Author: C. Bürger

/******** Global name space via '::' not supported yet => tests deactivated!

class C
{
public:
	class D
	{
	public:
		static void m(int v)
		{
			::C::D::v = v;
			//::E::v = v; // ::E::v not declared yet.
		};
		static int v;
	};
};

class E
{
public:
	static int v;
	static void m(int v)
	{
		::C::D::v = v;
	}
	class F;
};

class ::E::F
{
public:
};

//class E::F // Redefinition error.
//{
//public:
//};

********/

int main()
{
}
