package org.berlin.test;

import org.drools.KnowledgeBase;
import org.drools.KnowledgeBaseFactory;
import org.drools.builder.KnowledgeBuilder;
import org.drools.builder.KnowledgeBuilderFactory;
import org.drools.builder.ResourceType;
import org.drools.io.ResourceFactory;
import org.drools.logger.KnowledgeRuntimeLogger;
import org.drools.logger.KnowledgeRuntimeLoggerFactory;
import org.drools.runtime.StatefulKnowledgeSession;

public class Test1 {

	public static void main(final String[] args) throws Exception {

		final KnowledgeBuilder kbuilder = KnowledgeBuilderFactory.newKnowledgeBuilder();
		kbuilder.add(ResourceFactory.newClassPathResource("Fibonacci.drl", Test1.class), ResourceType.DRL);
		final KnowledgeBase kbase = KnowledgeBaseFactory.newKnowledgeBase();
		kbase.addKnowledgePackages(kbuilder.getKnowledgePackages());
		final StatefulKnowledgeSession ksession = kbase.newStatefulKnowledgeSession();
		KnowledgeRuntimeLogger logger = KnowledgeRuntimeLoggerFactory.newFileLogger(ksession, "log/fibonacci");
		ksession.insert(new Fibonacci(10));
		ksession.fireAllRules();
		logger.close();
		ksession.dispose(); 
	}

	public static class Fibonacci {
		private int sequence;

		private long value;

		public Fibonacci() {

		}

		public Fibonacci(final int sequence) {
			this.sequence = sequence;
			this.value = -1;
		}

		public int getSequence() {
			return this.sequence;
		}

		public void setValue(final long value) {
			this.value = value;
		}

		public long getValue() {
			return this.value;
		}

		public String toString() {
			return "Fibonacci(" + this.sequence + "/" + this.value + ")";
		}
	}

}
