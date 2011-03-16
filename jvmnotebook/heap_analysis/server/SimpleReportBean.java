/******************************************************************************* * 
 * Created On: 12/2/2008  
 ******************************************************************************/
package server;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

public class SimpleReportBean implements Serializable {

    /**
     * Specify a unique identification number for the serializable class. 
     */
    private static final long serialVersionUID = 3415594062848249479L;      
        
    private String someNo = "2342342342329323U290U32392908938200UPS0340_FEDEXRUNING2903-230923,KSDKLJLFSLKJKLDJFL28903I8203232LJ2L3J23-92309-23K0329038283028";         
    private Date sigDate  = new Date();    
    private String userId = "AB9303ARUNNING_KJSLDFLSJKDJFSKLDJLSKJDKLFJKLJSDKLFSJKLDJFKL";        
    
    private BigDecimal purchaseAmount = new BigDecimal(0);        
    private BigDecimal cPaid = new BigDecimal(0);
    private Date bDate = new Date();
    private Date dPropDate = new Date();
    private Date rDate = new Date();
    private String userTitle = "Very Important";          
    
    private String mailingAddr1 = "1023 Scary Hollow Ln";               
    private String mailingAddr2 = "Next to the dumpster, APT 030303030K";    
    private String mailingCity = "Austin";     
    private String mailingState = "Texas";         
    private String mailingZip = "90781";
        
    private String firstName = "Berlin";                       
    private String lastName = "Brown";
        
    private BigDecimal lAmount = new BigDecimal(0);;    
    private BigDecimal rlAmount = new BigDecimal(0);;           
        
    private BigDecimal r2Amount = new BigDecimal(0);
    private BigDecimal a2Amount = new BigDecimal(0);
    private BigDecimal p2Amount  = new BigDecimal(0);    
        
    private Date dateOfBirth = new Date();
           
    private String dispStatus = "Not Open";
    
    private String permitStateAbbr = "CA";
       
    public SimpleReportBean() { 
        this.setSomeNo("");
        this.setMailingAddr1("");
    }
    

    /**
     * @return the a2Amount
     */
    public BigDecimal getA2Amount() {
        return a2Amount;
    }

    /**
     * @param amount the a2Amount to set
     */
    public void setA2Amount(BigDecimal amount) {
        a2Amount = amount;
    }

    /**
     * @return the bDate
     */
    public Date getBDate() {
        return bDate;
    }

    /**
     * @param date the bDate to set
     */
    public void setBDate(Date date) {
        bDate = date;
    }

    /**
     * @return the cPaid
     */
    public BigDecimal getCPaid() {
        return cPaid;
    }

    /**
     * @param paid the cPaid to set
     */
    public void setCPaid(BigDecimal paid) {
        cPaid = paid;
    }

    /**
     * @return the dateOfBirth
     */
    public Date getDateOfBirth() {
        return dateOfBirth;
    }

    /**
     * @param dateOfBirth the dateOfBirth to set
     */
    public void setDateOfBirth(Date dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
    }

    /**
     * @return the dispStatus
     */
    public String getDispStatus() {
        return dispStatus;
    }

    /**
     * @param dispStatus the dispStatus to set
     */
    public void setDispStatus(String dispStatus) {
        this.dispStatus = dispStatus;
    }

    /**
     * @return the dPropDate
     */
    public Date getDPropDate() {
        return dPropDate;
    }

    /**
     * @param propDate the dPropDate to set
     */
    public void setDPropDate(Date propDate) {
        dPropDate = propDate;
    }

    /**
     * @return the firstName
     */
    public String getFirstName() {
        return firstName;
    }

    /**
     * @param firstName the firstName to set
     */
    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    /**
     * @return the lAmount
     */
    public BigDecimal getLAmount() {
        return lAmount;
    }

    /**
     * @param amount the lAmount to set
     */
    public void setLAmount(BigDecimal amount) {
        lAmount = amount;
    }

    /**
     * @return the lastName
     */
    public String getLastName() {
        return lastName;
    }

    /**
     * @param lastName the lastName to set
     */
    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    /**
     * @return the mailingAddr1
     */
    public String getMailingAddr1() {
        return mailingAddr1;
    }

    /**
     * @param mailingAddr1 the mailingAddr1 to set
     */
    public void setMailingAddr1(String mailingAddr1) {
         for (int i = 0; i < 20; i++) {
            this.mailingAddr1 += mailingAddr1 + "_ADD_MORE_DATA_TOID__SKJDKLFLDJFSDKFJSKDLFSJKLDFJKLSJDFKLSJLDFJLSKD";
        }
    }

    /**
     * @return the mailingAddr2
     */
    public String getMailingAddr2() {
        return mailingAddr2;
    }

    /**
     * @param mailingAddr2 the mailingAddr2 to set
     */
    public void setMailingAddr2(String mailingAddr2) {
        this.mailingAddr2 = mailingAddr2;
    }

    /**
     * @return the mailingCity
     */
    public String getMailingCity() {
        return mailingCity;
    }

    /**
     * @param mailingCity the mailingCity to set
     */
    public void setMailingCity(String mailingCity) {
        this.mailingCity = mailingCity;
    }

    /**
     * @return the mailingState
     */
    public String getMailingState() {
        return mailingState;
    }

    /**
     * @param mailingState the mailingState to set
     */
    public void setMailingState(String mailingState) {
        this.mailingState = mailingState;
    }

    /**
     * @return the mailingZip
     */
    public String getMailingZip() {
        return mailingZip;
    }

    /**
     * @param mailingZip the mailingZip to set
     */
    public void setMailingZip(String mailingZip) {
        this.mailingZip = mailingZip;
    }

    /**
     * @return the p2Amount
     */
    public BigDecimal getP2Amount() {
        return p2Amount;
    }

    /**
     * @param amount the p2Amount to set
     */
    public void setP2Amount(BigDecimal amount) {
        p2Amount = amount;
    }

    /**
     * @return the permitStateAbbr
     */
    public String getPermitStateAbbr() {
        return permitStateAbbr;
    }

    /**
     * @param permitStateAbbr the permitStateAbbr to set
     */
    public void setPermitStateAbbr(String permitStateAbbr) {
        this.permitStateAbbr = permitStateAbbr;
    }

    /**
     * @return the purchaseAmount
     */
    public BigDecimal getPurchaseAmount() {
        return purchaseAmount;
    }

    /**
     * @param purchaseAmount the purchaseAmount to set
     */
    public void setPurchaseAmount(BigDecimal purchaseAmount) {
        this.purchaseAmount = purchaseAmount;
    }

    /**
     * @return the r2Amount
     */
    public BigDecimal getR2Amount() {
        return r2Amount;
    }

    /**
     * @param amount the r2Amount to set
     */
    public void setR2Amount(BigDecimal amount) {
        r2Amount = amount;
    }

    /**
     * @return the rDate
     */
    public Date getRDate() {
        return rDate;
    }

    /**
     * @param date the rDate to set
     */
    public void setRDate(Date date) {
        rDate = date;
    }

    /**
     * @return the rlAmount
     */
    public BigDecimal getRlAmount() {
        return rlAmount;
    }

    /**
     * @param rlAmount the rlAmount to set
     */
    public void setRlAmount(BigDecimal rlAmount) {
        this.rlAmount = rlAmount;
    }

    /**
     * @return the sigDate
     */
    public Date getSigDate() {
        return sigDate;
    }

    /**
     * @param sigDate the sigDate to set
     */
    public void setSigDate(Date sigDate) {
        this.sigDate = sigDate;
    }

    /**
     * @return the someNo
     */
    public String getSomeNo() {
        return someNo;
    }

    /**
     * @param someNo the someNo to set
     */
    public void setSomeNo(String someNo) {                
        for (int i = 0; i < 10; i++) {
            this.someNo += someNo + "_ADD_MORE_DATA_TOID__SKJDKLFLDJFSDKFJSKDLFSJKLDFJKLSJDFKLSJLDFJLSKD";
        }
    }

    /**
     * @return the userId
     */
    public String getUserId() {
        return userId;
    }

    /**
     * @param userId the userId to set
     */
    public void setUserId(String userId) {
        this.userId = userId;
    }

    /**
     * @return the userTitle
     */
    public String getUserTitle() {
        return userTitle;
    }

    /**
     * @param userTitle the userTitle to set
     */
    public void setUserTitle(String userTitle) {
        this.userTitle = userTitle;
    }
        
}
