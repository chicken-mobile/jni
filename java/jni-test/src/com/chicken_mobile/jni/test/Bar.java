package com.chicken_mobile.jni.test;

import java.io.Serializable;



public class Bar {
	
	public int id;
	
	public static int CONSTANT = 36;
	
	protected Handler handler = new Handler() {
		@Override
		public int send(int i) {
			return 1;
		}
	};

	public Bar() {
	}
	
	public Bar(int i) {
		id = i;
	}

	public Bar(String s) {
	}
	
	public int ov1() {
		return 1;
	}
	
	public int ov1(String s) {
		return 2;
	}
	
	public int ov1(int i) {
		return 3;
	}
	
	public int ov1(Bar a) {
		return 4;
	}
	
	public int ov1(int n, String s) {
		return 5;
	}
	
	public int ov1(int n, Bar b) {
		return 6;
	}
	
	public int ov1(long i) {
		return 7;
	}
	
	public int ov1(short i) {
		return 8;
	}
	
	public int ov1(long a, int b) {
		return 9;
	}
	
	public int ov1(float i) {
		return 10;
	}
	
	public int ov1(double i) {
		return 11;
	}
	
	public int ov1(int a, long b) {
		return 12;
	}
	
	public int ov1(int a, int b) {
		return 13;
	}
	
	public int ov1(N1 n) {
		return 14;
	}
	
	public int ov1(N2 n) {
		return 15;
	}
	
	public int ov1(java.lang.Integer i) {
		return 16;
	}

	public int ov1(char c) {
		return 17;
	}

	public int ov2(int i) {
		return 1;
	}
	
	public String ov2(String s) {
		return "ov2";
	}
	
	public int ov2(Serializable s) {
		return 2;
	}
	
}
