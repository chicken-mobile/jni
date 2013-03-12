package com.chicken_mobile.jni;

import java.io.PrintWriter;
import java.io.StringWriter;

public class ExceptionHelper {

	public static String traceAsString(Exception e) {
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		e.printStackTrace(pw);
		return sw.toString();
	}
	
	public static String type(Exception e) {
		return e.getClass().getName();
	}
	
}
