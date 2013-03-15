package com.chicken_mobile.jni;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;

public class ReflectionHelper {
	
	public static Method[] findMethods(Class<?> c, String name) {
		ArrayList<Method> methods = new ArrayList<Method>();
		for (Method m : c.getMethods()) {
			if (m.getName().equals(name)) {
				methods.add(m);
			}
		}
		Method[] ms = new Method[methods.size()];
		return methods.toArray(ms);
	}
	
	public static Field findField(Class<?> c, String name) {
		try {
			return c.getField(name);
		} catch (NoSuchFieldException e) {
			return null;
		} catch (SecurityException e) {
			return null;
		}
	}
}