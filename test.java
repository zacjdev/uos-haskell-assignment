package backend;

import java.util.Date;
//Class Declaration

public class Property {
	// Instance Variables
	public void hasLivingFacility() {}
	public void hasBathingFacility() {}	
	public void hasKitchenFacility() {}
	public void hasUtilityFacility() {}
	public void hasSleepingFacility() {}
	public void hasOutdoorFacility() {}
	
	boolean breakfastAvailability;
	String shortName;
	String longName;
	public void approxAddress() {}
	public boolean isAvailable(Date startDate, Date endDate) {
		return true;//does something
	}
	
	// Living Facilities
	public boolean wifi;
	public boolean television;
	public boolean satellite;
	public boolean streaming;
	boolean dvdPlayer;
	boolean boardGames;
	
	// Bathing Facilities
	public boolean hairDryer;
	public boolean shampoo;
	public boolean toiletPaper;
	public int noOfBathrooms;
	int noOfToilets;
	
	// Bathing Facilities Bathroom
	public boolean toilet;
	public boolean bath;
	public boolean shower;
	public boolean shared;
	
	// Kitchen Facilities
	public boolean fridge;
	public boolean microwave;
	public boolean oven;
	public boolean stove;
	boolean dishwasher;
	boolean cookware;
	public boolean basicProvisions;
	
	// Utility Facilities
	public boolean centralHeating;
	public boolean washingMachine;
	public int dryingMachine;
	int fireExtinguisher;
	boolean smokeAlarm;
	public boolean firstAidKit;
	
	// Sleeping Facilities
	public boolean bedLinen;
	public boolean towels;
	boolean typeOfBed;
	int noOfRooms;
	
	// Sleeping Facilities Bedroom
	public String bedOneType;
	public String bedTwoType;
	
	public void SleepingCapacity() {}
	
	// Outdoor Facilities
	public boolean parkingAvailibility;
	public boolean patio;
	public boolean barbeque;


	// Constructor Declaration of Class
	
	public void hasLivingFacility(boolean television, boolean wifi, boolean satellite, boolean streaming, boolean dvdPlayer, boolean boardGames)
	{
		this.wifi = wifi;
		this.television = television;
		this.satellite = satellite;
		this.streaming = streaming;
		this.dvdPlayer = dvdPlayer;
		this.boardGames = boardGames;
	}
	
	public void hasBathingFacility(boolean hairDryer, boolean shampoo, boolean toiletPaper, int noOfBathrooms, int noOfToilets)
	{
		this.hairDryer = hairDryer;
		this.shampoo = shampoo;
		this.toiletPaper = toiletPaper;
		this.noOfBathrooms = noOfBathrooms;
		this.noOfToilets = noOfToilets;
	}
	
	public void hasBathingFacilityBathroom(boolean toilet, boolean bath, boolean shower, boolean shared)
	{
		this.toilet = toilet;
		this.bath = bath;
		this.shower = shower;
		this.shared = shared;
	}
	
	public void hasKitchenFacility(boolean fridge, boolean microwave, boolean oven, boolean stove, boolean dishwasher, boolean cookware, boolean basicProvisions)
	{
		this.fridge = fridge;
		this.microwave = microwave;
		this.oven = oven;
		this.stove = stove;
		this.dishwasher = dishwasher;
		this.cookware = cookware;
		this.basicProvisions = basicProvisions;
	}
	
	public void hasUtilityFacility(boolean centralHeating, boolean washingMachine, int dryingMachine, int fireExtinguisher, boolean smokeAlarm, boolean firstAidKit)
	{
		this.centralHeating = centralHeating;
		this.washingMachine = washingMachine;
		this.dryingMachine = dryingMachine;
		this.fireExtinguisher = fireExtinguisher;
		this.smokeAlarm = smokeAlarm;
		this.firstAidKit = firstAidKit;
	}
	
	public void hasSleepingFacility(boolean bedLinen, boolean towels, boolean typeOfBed, int noOfRooms)
	{
		this.bedLinen = bedLinen;
		this.towels = towels;
		this.typeOfBed = typeOfBed;
		this.noOfRooms = noOfRooms;
	}
	
	public void hasSleepingFacilityBedroom(String bedOneType, String bedTwoType)
	{
		this.bedOneType = bedOneType;
		this.bedTwoType = bedTwoType;
	}
	
	public void hasOutdoorFacility(boolean parkingAvailibility, boolean patio, boolean barbeque)
	{
		this.parkingAvailibility = parkingAvailibility;
		this.patio = patio;
		this.barbeque = barbeque;
	}
    // Make new property
    public static void main(String[] args) {
    	Property property = new Property();
    	property.hasLivingFacility(true, true, true, true, true, true);
    	property.hasBathingFacility(true, true, true, 1, 1);
    	property.hasBathingFacilityBathroom(true, true, true, true);
        property.hasKitchenFacility(true, true, true, true, true, true, true);
        property.hasUtilityFacility(true, true, 1, 1, true, true, true);
        property.hasSleepingFacility(true, true, true, 1);

}