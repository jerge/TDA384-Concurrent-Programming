import TSim.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;

public class Lab1 {

  TSimInterface tsi;
  private Train train1;
  private Train train2;

  Semaphore cross = new Semaphore(0);
  Semaphore up = new Semaphore(0);
  Semaphore left = new Semaphore(0);
  Semaphore right = new Semaphore(0);
  Semaphore down = new Semaphore(0);



  public Lab1(int speed1, int speed2) {
    tsi = TSimInterface.getInstance();
    train1 = new Train(1,tsi);
    train2 = new Train(2,tsi);
    try {
      init(speed1, speed2);
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
  }

  private void init(int speed1, int speed2) throws CommandException{
    train1.setSpeed(speed1);
    train2.setSpeed(speed2);
  }

  private class Train extends Thread{
    private int id;
    private TSimInterface tsim;
    // 1 is starting direction, -1 is backwards direction
    private int direction = 1;

    Train(int id, TSimInterface tsim) {
      this.id = id;
      this.tsim = tsim;
    }

    void setSpeed(int speed){
      try {
        tsim.setSpeed(id, speed);
      } catch (CommandException e) {
        e.printStackTrace();
        System.exit(1);
      }
    }

    void addSensorEvent(int xPos, int yPos, int status) {

    }
  }
}
