
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collections;
import java.util.LinkedList;
import java.util.Scanner;

public class CollectionSort {

	public static void main(String[] args) {
		LinkedList<Integer> list = new LinkedList<>();

		readFile(list, args[0]);
		createFile(list, args[1], Integer.valueOf(args[2]));
	}

	private static void readFile(LinkedList<Integer> list, String fileName) {
		try {
			File read = new File(fileName);
			Scanner sReader = new Scanner(read);
			while (sReader.hasNextLine()) {
				String line = sReader.nextLine();
				list.add(Integer.valueOf(line));
			}
			System.out.println(list);
			sReader.close();

		} catch (FileNotFoundException e) {
			System.out.println("Error reading file.");
		}
	}

	private static void createFile(LinkedList<Integer> list, String fileName, int N) {
		try {
			File write = new File(fileName);
			FileWriter fw = new FileWriter(write);

			write.createNewFile();
			fw.write("index,\t time" + System.lineSeparator());
			System.out.println("Computing execution time.");
			
			for (int i = 1; i <= N; i++) {
				LinkedList<Integer> copylist = new LinkedList<>(list);
				//System.out.println(copylist);
				long startTime = System.nanoTime();
				Collections.sort(copylist);
				long endTime = System.nanoTime();

				long time = endTime - startTime;
				fw.write(i + ",\t" + time + System.lineSeparator());
			}

			fw.close();

		} catch (IOException e) {
			System.out.println("Error creating file.");
		}
	}

}