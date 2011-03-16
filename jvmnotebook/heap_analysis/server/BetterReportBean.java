/******************************************************************************* * 
 * Created On: 12/2/2008  
 ******************************************************************************/
package server;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

public class BetterReportBean implements Serializable {

    /**
     * Specify a unique identification number for the serializable class. 
     */
    private static final long serialVersionUID = 3415594062848249479L;      
        
	public static final String a = "2342342342329323U290U32392908938200UPS0340_FEDEXRUNING2903-230923,KSDKLJLFSLKJKLDJFL28903I8203232LJ2L3J23-92309-23K0329038283028";         
	public static final String b = "AB9303ARUNNING_KJSLDFLSJKDJFSKLDJLSKJDKLFJKLJSDKLFSJKLDJFKL";        
	public static final String c = "Very Important";          
	public static final String d = "1023 Scary Hollow Ln";               
	public static final String ca = "CA";
	public static final BigDecimal bd = new BigDecimal(0);
	public static final Date dd = new Date();
	

    private String someNo = a;
    private Date sigDate  = dd;
    private final String userId = b;
    
    private BigDecimal purchaseAmount = bd;
    private BigDecimal cPaid = bd;
    private Date bDate = dd;
    private Date dPropDate = dd;
    private Date rDate = dd;
    private String userTitle = c;
    
    private String mailingAddr1 = d;
    private String mailingAddr2 = "Next to the dumpster, APT 030303030K";    
    private String mailingCity = "Austin";     
    private String mailingState = "Texas";         
    private String mailingZip = "90781";
        
    private String firstName = "Berlin";
    private String lastName = "Brown";
        
    private BigDecimal lAmount = bd;
    private BigDecimal rlAmount = bd;
        
    private BigDecimal r2Amount = bd;
    private BigDecimal a2Amount = bd;
    private BigDecimal p2Amount  = bd;
        
    private Date dateOfBirth = dd;
           
    private String dispStatus = "Not Open";
  
    private String permitStateAbbr = ca;
       
    public BetterReportBean() { 
        this.setSomeNo("");
        this.setMailingAddr1("");
    }
    

    /**
     * @return the a2Amount
     */
    final public BigDecimal getA2Amount() {
        return a2Amount;
    }

    /**
     * @param amount the a2Amount to set
     */
    final public void setA2Amount(BigDecimal amount) {
        a2Amount = amount;
    }

    /**
     * @return the bDate
     */
    final public Date getBDate() {
        return bDate;
    }

    /**
     * @param date the bDate to set
     */
    final public void setBDate(Date date) {
        bDate = date;
    }

    /**
     * @return the cPaid
     */
    final public BigDecimal getCPaid() {
        return cPaid;
    }

    /**
     * @param paid the cPaid to set
     */
    final public void setCPaid(BigDecimal paid) {
        cPaid = paid;
    }

    /**
     * @return the dateOfBirth
     */
    final public Date getDateOfBirth() {
        return dateOfBirth;
    }

    /**
     * @param dateOfBirth the dateOfBirth to set
     */
    final public void setDateOfBirth(Date dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
    }

    /**
     * @return the dispStatus
     */
    final public String getDispStatus() {
        return dispStatus;
    }

    /**
     * @param dispStatus the dispStatus to set
     */
    final public void setDispStatus(String dispStatus) {
        this.dispStatus = dispStatus;
    }

    /**
     * @return the dPropDate
     */
    final public Date getDPropDate() {
        return dPropDate;
    }

    /**
     * @param propDate the dPropDate to set
     */
    final public void setDPropDate(Date propDate) {
        dPropDate = propDate;
    }

    /**
     * @return the firstName
     */
    final public String getFirstName() {
        return firstName;
    }

    /**
     * @param firstName the firstName to set
     */
    final public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    /**
     * @return the lAmount
     */
    final public BigDecimal getLAmount() {
        return lAmount;
    }

    /**
     * @param amount the lAmount to set
     */
    final public void setLAmount(BigDecimal amount) {
        lAmount = amount;
    }

    /**
     * @return the lastName
     */
    final public String getLastName() {
        return lastName;
    }

    /**
     * @param lastName the lastName to set
     */
    final public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    /**
     * @return the mailingAddr1
     */
    final public String getMailingAddr1() {
        return mailingAddr1;
    }

    /**
     * @param mailingAddr1 the mailingAddr1 to set
     */
    final public void setMailingAddr1(String mailingAddr1) {
		StringBuffer buf = new StringBuffer(2000);
		for (int i = 0; i < 20; i++) {
            buf.append(mailingAddr1).append(mailingAddr1).append("_ADD_MORE_DATA_TOID__SKJDKLFLDJFSDKFJSKDLFSJKLDFJKLSJDFKLSJLDFJLSKD");
        }
    }

    /**
     * @return the mailingAddr2
     */
    final public String getMailingAddr2() {
        return mailingAddr2;
    }

    /**
     * @param mailingAddr2 the mailingAddr2 to set
     */
    final public void setMailingAddr2(String mailingAddr2) {
        this.mailingAddr2 = mailingAddr2;
    }

    /**
     * @return the mailingCity
     */
    final public String getMailingCity() {
        return mailingCity;
    }

    /**
     * @param mailingCity the mailingCity to set
     */
    final public void setMailingCity(String mailingCity) {
        this.mailingCity = mailingCity;
    }

    /**
     * @return the mailingState
     */
    final public String getMailingState() {
        return mailingState;
    }

    /**
     * @param mailingState the mailingState to set
     */
    final public void setMailingState(String mailingState) {
        this.mailingState = mailingState;
    }

    /**
     * @return the mailingZip
     */
    final public String getMailingZip() {
        return mailingZip;
    }

    /**
     * @param mailingZip the mailingZip to set
     */
    final public void setMailingZip(String mailingZip) {
        this.mailingZip = mailingZip;
    }

    /**
     * @return the p2Amount
     */
    final public BigDecimal getP2Amount() {
        return p2Amount;
    }

    /**
     * @param amount the p2Amount to set
     */
    final public void setP2Amount(BigDecimal amount) {
        p2Amount = amount;
    }

    /**
     * @return the permitStateAbbr
     */
    final public String getPermitStateAbbr() {
        return permitStateAbbr;
    }

    /**
     * @param permitStateAbbr the permitStateAbbr to set
     */
    final public void setPermitStateAbbr(String permitStateAbbr) {
        this.permitStateAbbr = permitStateAbbr;
    }

    /**
     * @return the purchaseAmount
     */
    final public BigDecimal getPurchaseAmount() {
        return purchaseAmount;
    }

    /**
     * @param purchaseAmount the purchaseAmount to set
     */
    final public void setPurchaseAmount(BigDecimal purchaseAmount) {
        this.purchaseAmount = purchaseAmount;
    }

    /**
     * @return the r2Amount
     */
    final public BigDecimal getR2Amount() {
        return r2Amount;
    }

    /**
     * @param amount the r2Amount to set
     */
    final public void setR2Amount(BigDecimal amount) {
        r2Amount = amount;
    }

    /**
     * @return the rDate
     */
    final public Date getRDate() {
        return rDate;
    }

    /**
     * @param date the rDate to set
     */
    final public void setRDate(Date date) {
        rDate = date;
    }

    /**
     * @return the rlAmount
     */
    final public BigDecimal getRlAmount() {
        return rlAmount;
    }

    /**
     * @param rlAmount the rlAmount to set
     */
    final public void setRlAmount(BigDecimal rlAmount) {
        this.rlAmount = rlAmount;
    }

    /**
     * @return the sigDate
     */
    final public Date getSigDate() {
        return sigDate;
    }

    /**
     * @param sigDate the sigDate to set
     */
    final public void setSigDate(Date sigDate) {
        this.sigDate = sigDate;
    }

    /**
     * @return the someNo
     */
    final public String getSomeNo() {
        return someNo;
    }

    /**
     * @param someNo the someNo to set
     */
    final public void setSomeNo(String someNo) {                
		StringBuffer buf = new StringBuffer(3000);
        for (int i = 0; i < 10; i++) {
            buf.append(this.someNo).append(someNo).append("_ADD_MORE_DATA_TOID__SKJDKLFLDJFSDKFJSKDLFSJKLDFJKLSJDFKLSJLDFJLSKD");
        }
    }

    /**
     * @return the userId
     */
    final public String getUserId() {
        return userId;
    }

    /**
     * @return the userTitle
     */
    final public String getUserTitle() {
        return userTitle;
    }

    /**
     * @param userTitle the userTitle to set
     */
    final public void setUserTitle(String userTitle) {
        this.userTitle = userTitle;
    }
        
}
