package com.chicken_mobile.jni.test;



public class Foo {
	
	public static boolean lie = true;
	public static String noSense = "lil oiuy pppq";
 	public static String sense = "cogito ergo sum";
	public int number;
	@SuppressWarnings("unused")
	private int secret_number;

	public Foo() {
		number = 12;
		secret_number = 8;  //¡¡¡¡¡¡¡¡ oohhh  !!!!!!
	}
	
	public void xxx() {
		throw new RuntimeException("bad protocol");
	}
	
	public String xxx2() {
		throw new RuntimeException("bad protocol");
	}
}
